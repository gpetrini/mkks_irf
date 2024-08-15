#******************************************************************
#
# ----------- AB-SFC-SSM Impulse-response function analysis ------------
#
#   Based on script writen by Marcelo C. Pereira, University of Campinas
#              Marco Amendola, University of Napoli
#
#   Copyright Marcelo C. Pereira & Marco Amendola
#   Distributed under the GNU General Public License
#
#   The default configuration assumes that the supplied LSD
#   simulation configurations (basename Sx):
#     ./data-irf/SX.lsd
#   are executed before this script is used.
#
#******************************************************************

#******************************************************************
#
# ------------ Read Monte Carlo experiment files ----------------
#
#******************************************************************

library( LSDirf )
# remove warnings for support functions and saved data
# !diagnostics suppress = irf.lsd, state.irf.lsd

library(parallel)
library(doParallel)
library(foreach)

# Set up parallel backend
numCores <- detectCores() - 1  # Use one less than the total number of cores
cl <- makeCluster(numCores)
registerDoParallel(cl)


folder    <- "data-irf"                 # data files folder
rep_folder <- "data-irf/figs/"
baseName  <- "S3"                  # data files base name (no-shock/shock:1/2)
objects_folder <- "data-irf/objects/"
iniDrop   <- 0                    # initial time steps to drop (0=none)
nKeep     <- -1                     # number of time steps to keep (-1=all)
mcStat    <- "median"               # Monte Carlo statistic ("mean", "median")

Scenarios <- list(
  Baseline = list(),
  rShockTmp = list()
  ## rShockPer = list()
)

exps <- length(Scenarios)
MCdata <- Scenarios

MCdata[["Baseline"]][["Flag"]] <- 1
MCdata[["Baseline"]][["shockVar"]] <- NULL
MCdata[["rShockTmp"]][["shockVar"]] <- "rShock"
## MCdata[["rShockPer"]][["shockVar"]] <- "rShock"

readVars <- c(
    "_I", "_K", "_Y", "_Profits", "_AssetsF", "_NI", "_KN", "_P", "_FFI", "_Lev", "_inf", "_Markup", "_u",
    "_ProfitRate", "_gA", "_gY", "_PDIndex", "_Minskian", "_HHI", "BaseInterestRate"
)


## aggrVars <- readRDS(paste0("./", objects_folder, "/", baseName, "_Variables", ".rds"))

recycle_res <- FALSE
## recycle_res <- TRUE
no_score_names <- readVars
no_score_names <- stringr::str_remove( string = no_score_names, "^__" )
no_score_names <- stringr::str_remove( string = no_score_names, "^_" )



# potential state-defining variables to consider, including added variables
# NOTE: Used latter, so no underscore is required
stateVars <- c( "FFI", "u", "Minskian", "PDIndex", "Markup", "BaseInterestRate", "Lev" )

irfVars <- c("Y")


# ==== Process LSD result files ====

# load support packages and functions
source( "KS-support-functions.R" )

shockVar <- "rShock" ## FIXME generalize
refVar <- "Y" ## FIXME generalize
mc <- read.3d.lsd( list.files.lsd( folder, paste0( baseName, "-noshock" ), sensitivity = TRUE  ),
                   c(shockVar, readVars, paste0("X_", irfVars[1]), paste0("X_",refVar )),
                   skip = iniDrop, nrows = nKeep, nnode = -1)
dimnames(mc)[[2]] <- stringr::str_remove( string = dimnames(mc)[[2]], "X__" )
dimnames(mc)[[2]] <- stringr::str_remove( string = dimnames(mc)[[2]], "X_" )
MCdata[[1]][["MC"]] <- mc
MCdata[[1]][["NAME"]] <- names(Scenarios)[1]

mcShock <- read.3d.lsd( list.files.lsd( folder, paste0( baseName, "-shock" ), sensitivity = TRUE ),
                        c(shockVar, readVars, paste0("X_", irfVars[1]), paste0("X_",refVar )),
                        skip = iniDrop, nrows = nKeep, nnode = -1 )
dimnames(mcShock)[[2]] <- stringr::str_remove( string = dimnames(mcShock)[[2]], "X__" )
dimnames(mcShock)[[2]] <- stringr::str_remove( string = dimnames(mcShock)[[2]], "X_" )
MCdata[[2]][["MC"]] <- mcShock
MCdata[[2]][["NAME"]] <- names(Scenarios)[2]

invisible(gc(verbose = FALSE))




#******************************************************************
#
# --------------------- Plot statistics -------------------------
#
#******************************************************************

# ===================== User parameters =========================

top_States <- 3 ### Top N states for the state IRF
max_States <- 10

irfHor    <- 40                     # time horizon to compute IRF
irfRel    <- TRUE                   # F=absolute deviation, T=relative deviation

limOutl   <- 3                      # limit threshold multiple for outliers (0=off)

bootAlpha <- 0.05                   # bootstrap confidence interval significance
bootR     <- 999                    # bootstrap confidence interval replicates
bootCI    <- "basic"                # bootstrap confidence interval method
                                    # ("basic" or "bca")

treeN     <- 1000                   # number of trees in random forest
treeDep   <- 2                      # maximum depth of random trees
nodeMin   <- 30                     # final node min number of observations
varTry    <- 2                      # number of variables to try/sample per node
alpha     <- 0.05                   # significance for node differences
quantile  <- 10                     # number of discrete state quantiles

repName   <- ""                     # report files base name (if "" same baseName)
sDigits   <- 4                      # significant digits in tables
plotRows  <- 1                      # number of plots per row in a page
plotCols  <- 1                      # number of plots per column in a page
plotW     <- 10                     # plot window width
plotH     <- 7                      # plot window height


# ====== Functions to process dataset ======

# function to define IRF states according to the value of state variable(s)
evalState <- function( data ) {

  # vector of probabilities (quantiles) to split data
  n <- 2
  probs <- seq( 0, 1, 1 / n )
  dataQuant <- quantile( data[ , stateVar ], probs, na.rm = TRUE, type = 8 )

  return( findInterval( data[ , stateVar ], dataQuant, all.inside = TRUE ) )
}


# function to compute IRF metric (higher values mean better performance)
irfMetric <- function( data ) {

  # cumulative irf time weights in reverse order (0 if not set)
  irfWght <- c( 1, 1, 1, 1 )

  metric <- rep( 0, nrow( data ) )
  irfWght <- irfWght[ 1 : min( ncol( data ), length( irfWght ) ) ]

  for( i in 1 : length( irfWght ) )
    metric <- metric - irfWght[ i ] * data[ , irfHor - i + 1 ]

  metric <- metric / sum( irfWght )

  return( metric )
}






# ==== Support stuff ====

if( repName == "" ) repName <- baseName
if( irfRel ) irType <- "Relative" else irType <- "Absolute"


data_baseline_mc <- MCdata[[1]][["MC"]]

df <- expand.grid(Scen = names(Scenarios[-1]), Var = irfVars, stringsAsFactors = FALSE)

# ====== Analyze IRF data ======
results <- foreach(
  k = 1 : nrow(df),
  .packages = c("LSDirf")
) %dopar% {

  scen <- df[["Scen"]][k]
  resp <- df[["Var"]][k]
  scen_id <- grep(scen, names(Scenarios))

  data_shock_mc <- MCdata[[scen_id]][["MC"]]
  shock_var <- MCdata[[scen_id]][["shockVar"]]

  result <- list(
    LINEAR = NULL,
    NAME = NULL,
    STATE = list(),
    IDENT = NULL,
    SENSITIVITY = NULL,
    TOPSTATE = list()
  )
  result$NAME <- scen
  linearIRF <- irf.lsd(
    data = data_baseline_mc,
    data.shock = data_shock_mc,
    t.horiz = irfHor,
    var.irf = resp,
    var.shock = shock_var,
    var.ref = NULL,
    irf.type = "none",
    stat = mcStat,
    ci.R = bootR,
    ci.type = bootCI,
    lim.outl = limOutl,
    alpha = bootAlpha
  )
  result$LINEAR <- linearIRF


  for(stateVar in stateVars){
    stateIRF <- state.irf.lsd(
      data = data_baseline_mc,
      irf = linearIRF,
      state.vars = stateVar,
      eval.state = evalState,
      metr.irf = irfMetric,
      add.vars = NULL,
      irf.type = "none",
      ci.R = bootR,
      ci.type = bootCI,
      alpha = bootAlpha
    )
    result$STATE[[stateVar]] <- stateIRF
  }

  stateIdent <- state.ident.lsd(
    data = data_baseline_mc,
    irf = linearIRF,
    state.vars = stateVars, # MC state variables to consider
    metr.irf = irfMetric,   # function to compare C-IR's
    add.vars = NULL,     # function to add new variables
    ntree = treeN,          # number of trees in random forest
    maxdepth = treeDep,     # maximum depth of random trees
    nodesize = nodeMin,     # final node min number of observations
    mtry = varTry,          # number of variable samples per node
    alpha = alpha,          # significance for node differences
    quantile = quantile )   # number of discrete state quantiles

  result$IDENT <- stateIdent


  stateSens <- state.sa.lsd(
    data = data_baseline_mc,
    irf = linearIRF,
    state.vars = stateVars,
    metr.irf = irfMetric,
    add.vars = NULL,
    ntree = treeN,
    nodesize = nodeMin,
    mtry = varTry,
    alpha = alpha,
    no.plot = TRUE
  )
  result$SENSITIVITY <- stateSens


  for(i in 1:top_States){
    txt <- paste0("STATE", i)
    stateIRFTop <- state.irf.lsd(
      data = data_baseline_mc,
      irf = linearIRF,
      states = stateIdent,
      state.num = i,
      metr.irf = irfMetric,
      add.vars = NULL,
      irf.type = "none",
      ci.R = bootR,
      ci.type = bootCI,
      alpha = bootAlpha
    )
    result$TOPSTATE[[txt]] <- stateIRFTop

  }



  return(result)
}

res_list <- vector("list", length = length(Scenarios))
for(k in 1:nrow(df)){

  scen <- df[["Scen"]][k]
  resp <- df[["Var"]][k]
  res <- results[[k]]
  scen_id <- grep(scen, names(Scenarios))
  res_list[[scen_id]][[resp]] <- res


}

for(i in 2:length(res_list)){
  obj_irf <- paste0("./", objects_folder, "/", baseName, i, "_irf", ".rds")
  saveRDS(res_list[[i]], file = obj_irf)
}

saveRDS(res_list, file = paste0("./", objects_folder, "/", baseName, "_irf_All.rds"))

stopCluster(cl)

for(i in 2:length(res_list)){

  scen <- names(res_list)[i]
  scen_id <- i
  scen_res <- res_list[[scen_id]]
                                        # ==== Create PDF ====
  pdf( paste0( rep_folder, "/", repName, scen_id, "_irf_",  mcStat, ".pdf" ), width = plotW, height = plotH )
  par( mfrow = c ( plotRows, plotCols ) )


  xlab <- "Relative time after shock"
  for(resp in irfVars){

    col <- "red"

    tmp_lin <- scen_res[[resp]][["LINEAR"]]

    plot( tmp_lin, irf.type = "incr.irf", scale = 2, center = TRUE, col = col,
         lwd = 2, lty.ci = 2,  xlab = xlab, ylab = "Impulse response",
         main = paste( "Linear impulse-response function for", resp ),
         sub = paste( "( MC sample =", tmp_lin$nsample, "/ MC", mcStat,
                     "/ CI signif. =",  tmp_lin$alpha, ")" ) )

    plot( tmp_lin, irf.type = "cum.irf", scale = 1, center = FALSE, col = col,
         lwd = 2, lty.ci = 2,  xlab = xlab, ylab = "Cumulative impulse response",
         main = paste( "Linear cumulative impulse-response function for", resp ),
         sub = paste( "( MC sample =", tmp_lin$nsample, "/ MC", mcStat,
                     "/ CI signif. =",  tmp_lin$alpha, ")" ) )


    ## plot( tmp_lin, irf.type = "peak.mult", scale = 2, center = TRUE, col = col,
    ##      lwd = 2, lty.ci = 2,  xlab = xlab, ylab = "Peak impulse-multiplier",
    ##      main = paste( "Linear peak impulse-multiplier function for", resp ),
    ##      sub = paste( "( MC sample =", tmp_lin$nsample, "/ MC", mcStat,
    ##                  "/ CI signif. =",  tmp_lin$alpha, ")" ) )

    ## plot( tmp_lin, irf.type = "cum.mult", scale = 1, center = FALSE, col = col,
    ##      lwd = 2, lty.ci = 2,  xlab = xlab, ylab = "Cumulative impulse-multiplier",
    ##      main = paste( "Linear cumulative impulse-multiplier function for", resp ),
    ##      sub = paste( "( MC sample =", tmp_lin$nsample, "/ MC", mcStat,
    ##                  "/ CI signif. =",  tmp_lin$alpha, ")" ) )

    for(state in stateVars){

      col <- c( "green", "blue" )

      tmp_state <- scen_res[[resp]][["STATE"]][[state]]

      plot( tmp_state, state = 0, irf.type = "incr.irf", scale = 1, center = TRUE,
           col = col, lwd = 2, col.ci = col, xlab = xlab,
           ylab = "Impulse response by state",
           main = paste( "State-dependent impulse-response functions for", resp ),
           sub = paste( "( MC sample =", tmp_state$nsample, "/ MC", mcStat,
                       "/ CI signif. =",  tmp_lin$alpha,
                       "/ U-test p-val =", signif( tmp_state$irf.test$p.value, 2 ),
                       "/ H0: similar states )" ),
           leg = c( paste( "Low", state, "state" ),
                   paste( "High", state, "state" ) ) )

      plot( tmp_state, state = 0, irf.type = "cum.irf", scale = 1, center = FALSE,
           col = col, lwd = 2, col.ci = col, xlab = xlab,
           ylab = "Cumulative impulse response by state",
           main = paste( "State-dependent cumulative impulse-response functions for", resp ),
           sub = paste( "( MC sample =", tmp_state$nsample, "/ MC", mcStat,
                       "/ CI signif. =", tmp_lin$alpha,
                       "/ U-test p-val =", signif( tmp_state$cirf.test$p.value, 2 ),
                       "/ H0: similar states )" ),
           leg = c( paste( "Low", state, "state" ),
                   paste( "High", state, "state" ) ) )
    }

    tmp_top <- scen_res[[resp]][["TOPSTATE"]]
    for(j in 1:top_States){

      txt <- paste0("STATE", j)
      tmp_irf <- tmp_top[[txt]]

      plot( tmp_irf,
           state = 0, irf.type = "incr.irf", scale = 1, center = TRUE,
           col = col, lwd = 2, col.ci = col, xlab = xlab,
           ylab = "Impulse response by state",
           main = paste( "State-dependent IRF for", resp, "for", tmp_irf$state ),
           sub = paste( "( MC sample =", tmp_irf$nsample, "/ MC", mcStat,
                       "/ CI signif. =",  tmp_lin$alpha,
                       "/ U-test p-val =", signif( tmp_irf$irf.test$p.value, 2 ),
                       "/ H0: similar states )" ),
           leg = c( paste( "Low", state, "state" ),
                   paste( "High", state, "state" ) ) )

      plot( tmp_irf, state = 0, irf.type = "cum.irf", scale = 1, center = FALSE,
           col = col, lwd = 2, col.ci = col, xlab = xlab,
           ylab = "Cumulative impulse response by state",
           main = paste( "State-dependent C-IRF for", resp, "for", tmp_irf$state ),
           sub = paste( "( MC sample =", tmp_irf$nsample, "/ MC", mcStat,
                       "/ CI signif. =", tmp_lin$alpha,
                       "/ U-test p-val =", signif( tmp_irf$cirf.test$p.value, 2 ),
                       "/ H0: similar states )" ),
           leg = c( paste( "Low", state, "state" ),
                   paste( "High", state, "state" ) ) )
    }

                                        #
                                        # ------ Show detected important states ------
                                        #

    tmp_ident <- scen_res[[resp]][["IDENT"]]

    textplot( format( tmp_ident$state.freq[ 1 : max_States, ], digits = sDigits ),
             cmar = 1, show.rownames = FALSE )
    title( main = paste( "Top", max_States, "discrete-state frequency for", resp,"(",
                        tmp_ident$var.ref, "C-IRF )" ),
          sub = paste( "( MC sample =", tmp_ident$nsample, "/ Tree depth =",
                      tmp_ident$maxdepth, "/ Significance =",
                      tmp_ident$alpha, "/ Quantiles =",
                      tmp_ident$quantile, " )" ) )


                                        #
                                        # ------ Random forest sensitivity analysis ------
                                        #

    tmp_sens <- scen_res[[resp]][["SENSITIVITY"]]
    plot( tmp_sens, xlab = "State-defining variables",
         main = paste( "Sensitivity analysis of state-defining variables (", resp, "IRF )" ),
         sub = paste( "( MC sample =", tmp_sens$nsample, "/ MC", mcStat,
                     "/ Pseudo R2 =", signif( tmp_sens$rsq, digits = 2 ),
                     "/ CI signif. =", tmp_sens$alpha, ")" ) )


  }
                                        # close plot file
  dev.off( )
}
