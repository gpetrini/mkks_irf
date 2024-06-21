
#******************************************************************
#
# ----------- MKKS Impulse-response function analysis ------------
#
#   Based on script writen by Marcelo C. Pereira, University of Campinas
#              Marco Amendola, University of Napoli
#
#   Copyright Marcelo C. Pereira & Marco Amendola
#   Distributed under the GNU General Public License
#
#   The default configuration assumes that the supplied LSD
#   simulation configurations (basename Sx):
#     ../data-irf/Sx-noshock.lsd
#     ../data-irf/Sx-shock.lsd
#   are executed before this script is used.
#
#******************************************************************

#******************************************************************
#
# ------------ Read Monte Carlo experiment files ----------------
#
#******************************************************************

folder    <- "data-irf"                 # data files folder
baseName  <- "S1"                  # data files base name (no-shock/shock:1/2)
iniDrop   <- 100                    # initial time steps to drop (0=none)
nKeep     <- -1                     # number of time steps to keep (-1=all)
mcStat    <- "median"               # Monte Carlo statistic ("mean", "median")

irfVar    <- "Y"              # variable to compute impulse-response fun.
refVar    <- "Y"              # reference var. to compute IRF as share (%)
shockVar  <- "rShock"               # shock variable name
stateVar  <- "Lev"                  # state variable to be used


# LSD original variables to read from files
readVars <- c(
    "_I", "_K", "_Y", "_Profits", "_AssetsF", "_NI", "_KN", "_P", "_FFI", "_Lev", "_inf", "_Markup", "_u",
    "_ProfitRate", "_gA", "_gY", "_PDIndex", "_Minskian", "_HHI"
)

# potential state-defining variables to consider, including added variables
# NOTE: Used latter, so no underscore is required
stateVars <- c( "FFI", "u", "Minskian", "PDIndex", "Markup", "BaseInterestRate", "Lev" )


# ==== Process LSD result files ====

# load support packages and functions
source( "KS-support-functions.R" )

# ---- Read data files ----

mc <- read.3d.lsd( list.files.lsd( folder, paste0( baseName, "-noshock" ) ),
                   c( readVars, irfVar, refVar ),
                   skip = iniDrop, nrows = nKeep )
browser()
mcShock <- read.3d.lsd( list.files.lsd( folder, paste0( baseName, "-shock" ) ),
                        c( shockVar, irfVar ),
                        skip = iniDrop, nrows = nKeep )

dimnames(mcData[[1]])[[2]] <- stringr::str_remove( string = dimnames(mcData[[1]])[[2]], "X__" )
dimnames(mcData[[1]])[[2]] <- stringr::str_remove( string = dimnames(mcData[[1]])[[2]], "X_" )


#******************************************************************
#
# --------------------- Plot statistics -------------------------
#
#******************************************************************

# ===================== User parameters =========================

irfHor    <- 20                     # time horizon to compute IRF
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

# function to add new state variables based on existing ones (readVars)
addVars <- function( data ) {

  ## data$PiFgdp <- ( data$Pi1 + data$Pi2 ) / data$GDPnom
  ## data$NWbGDP <- data$NWb / data$GDPnom

  return( data )
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


# ====== External support functions & definitions ======

library( LSDirf )

# remove warnings for support functions and saved data
# !diagnostics suppress = irf.lsd, state.irf.lsd


# ==== Support stuff ====

if( repName == "" ) repName <- baseName
if( irfRel ) irType <- "Relative" else irType <- "Absolute"


# ====== Analyze IRF data ======

linearIRF <- irf.lsd( data = mc,              # non-shocked MC data
                      data.shock = mcShock,   # shocked data
                      t.horiz = irfHor,       # post-shock analysis time horizon
                      var.irf = irfVar,       # variable to compute IRF
                      var.shock = shockVar,   # shock variable (impulse)
                      var.ref = refVar,       # reference variable to IR measure
                      irf.type = "none",      # no plot now
                      stat = mcStat,          # type of statistic to use
                      ci.R = bootR,           # CI bootstrap repetitions (odd)
                      ci.type = bootCI,       # CI algorithm type
                      lim.outl = limOutl,     # outlier limit/threshold
                      alpha = bootAlpha )     # confidence interval conf. level

stateIRF <- state.irf.lsd(
                      data = mc,              # non-shock MC data
                      irf = linearIRF,        # linear IRF produced by irf.lsd()
                      state.vars = stateVar,  # variable defining states
                      eval.state = evalState, # function to evaluate state(s)
                      metr.irf = irfMetric,   # function to compare C-IR's
                      add.vars = addVars,     # function to add new variables
                      irf.type = "none",      # no plot now
                      ci.R = bootR,           # CI bootstrap repetitions (odd)
                      ci.type = bootCI,       # CI algorithm type
                      alpha = bootAlpha )     # confidence interval conf. level


# ====== Random-forest state identification ======

## stateIdent <- state.ident.lsd(
##                       data = mc,              # non-shock MC data
##                       irf = linearIRF,        # linear IRF produced by irf.lsd()
##                       state.vars = stateVars, # MC state variables to consider
##                       metr.irf = irfMetric,   # function to compare C-IR's
##                       add.vars = addVars,     # function to add new variables
##                       ntree = treeN,          # number of trees in random forest
##                       maxdepth = treeDep,     # maximum depth of random trees
##                       nodesize = nodeMin,     # final node min number of observations
##                       mtry = varTry,          # number of variable samples per node
##                       alpha = alpha,          # significance for node differences
##                       quantile = quantile )   # number of discrete state quantiles

## stateSens <- state.sa.lsd(
##                       data = mc,              # non-shock MC data
##                       irf = linearIRF,        # linear IRF produced by irf.lsd()
##                       state.vars = stateVars, # MC state variables to consider
##                       metr.irf = irfMetric,   # function to compare C-IR's
##                       add.vars = addVars,     # function to add new variables
##                       ntree = treeN,          # number of trees in random forest
##                       nodesize = nodeMin,     # final node min number of observations
##                       mtry = varTry,          # number of variable samples per node
##                       alpha = alpha,          # significance for node differences
##                       no.plot = TRUE )        # do not plot yet


# ====== Analyze state-dependent IRF of top 3 identified states ======

## stateIRF1 <- state.irf.lsd(
##                       data = mc,              # non-shock MC data
##                       irf = linearIRF,        # linear IRF produced by irf.lsd()
##                       states = stateIdent,    # object with identified states
##                       state.num = 1,          # number of identified state to analyze
##                       metr.irf = irfMetric,   # function to compare C-IR's
##                       add.vars = addVars,     # function to add new variables
##                       irf.type = "none",      # no plot now
##                       ci.R = bootR,           # CI bootstrap repetitions (odd)
##                       ci.type = bootCI,       # CI algorithm type
##                       alpha = bootAlpha )     # confidence interval conf. level

## stateIRF2 <- state.irf.lsd(
##                       data = mc,              # non-shock MC data
##                       irf = linearIRF,        # linear IRF produced by irf.lsd()
##                       states = stateIdent,    # object with identified states
##                       state.num = 2,          # number of identified state to analyze
##                       metr.irf = irfMetric,   # function to compare C-IR's
##                       add.vars = addVars,     # function to add new variables
##                       irf.type = "none",      # no plot now
##                       ci.R = bootR,           # CI bootstrap repetitions (odd)
##                       ci.type = bootCI,       # CI algorithm type
##                       alpha = bootAlpha )     # confidence interval conf. level

## stateIRF3 <- state.irf.lsd(
##                       data = mc,              # non-shock MC data
##                       irf = linearIRF,        # linear IRF produced by irf.lsd()
##                       states = stateIdent,    # object with identified states
##                       state.num = 3,          # number of identified state to analyze
##                       metr.irf = irfMetric,   # function to compare C-IR's
##                       add.vars = addVars,     # function to add new variables
##                       irf.type = "none",      # no plot now
##                       ci.R = bootR,           # CI bootstrap repetitions (odd)
##                       ci.type = bootCI,       # CI algorithm type
##                       alpha = bootAlpha )     # confidence interval conf. level


# ==== Create PDF ====

pdf( paste0( folder, "/", repName, "_irf_", irfVar, "_", stateVar, "_",  mcStat,
             ".pdf" ), width = plotW, height = plotH )
par( mfrow = c ( plotRows, plotCols ) )


#
# ------ Plot linear IRF ------
#

xlab <- "Relative time after shock"
col <- "red"

plot( linearIRF, irf.type = "incr.irf", scale = 2, center = TRUE, col = col,
      lwd = 2, lty.ci = 2,  xlab = xlab, ylab = "Impulse response",
      main = paste( "Linear impulse-response function for", irfVar ),
      sub = paste( "( MC sample =", linearIRF$nsample, "/ MC", mcStat,
                   "/ CI signif. =",  linearIRF$alpha, ")" ) )

plot( linearIRF, irf.type = "cum.irf", scale = 1, center = FALSE, col = col,
      lwd = 2, lty.ci = 2,  xlab = xlab, ylab = "Cumulative impulse response",
      main = paste( "Linear cumulative impulse-response function for", irfVar ),
      sub = paste( "( MC sample =", linearIRF$nsample, "/ MC", mcStat,
                   "/ CI signif. =",  linearIRF$alpha, ")" ) )

plot( linearIRF, irf.type = "peak.mult", scale = 2, center = TRUE, col = col,
      lwd = 2, lty.ci = 2,  xlab = xlab, ylab = "Peak impulse-multiplier",
      main = paste( "Linear peak impulse-multiplier function for", irfVar ),
      sub = paste( "( MC sample =", linearIRF$nsample, "/ MC", mcStat,
                   "/ CI signif. =",  linearIRF$alpha, ")" ) )

plot( linearIRF, irf.type = "cum.mult", scale = 1, center = FALSE, col = col,
      lwd = 2, lty.ci = 2,  xlab = xlab, ylab = "Cumulative impulse-multiplier",
      main = paste( "Linear cumulative impulse-multiplier function for", irfVar ),
      sub = paste( "( MC sample =", linearIRF$nsample, "/ MC", mcStat,
                   "/ CI signif. =",  linearIRF$alpha, ")" ) )


#
# ------ Plot state-dependent IRFs ------
#

## col <- c( "green", "blue" )

## plot( stateIRF, state = 0, irf.type = "incr.irf", scale = 1, center = TRUE,
##       col = col, lwd = 2, col.ci = col, xlab = xlab,
##       ylab = "Impulse response by state",
##       main = paste( "State-dependent impulse-response functions for", irfVar ),
##       sub = paste( "( MC sample =", stateIRF$nsample, "/ MC", mcStat,
##                    "/ CI signif. =",  linearIRF$alpha,
##                    "/ U-test p-val =", signif( stateIRF$irf.test$p.value, 2 ),
##                    "/ H0: similar states )" ),
##       leg = c( paste( "Low", stateVar, "state" ),
##                paste( "High", stateVar, "state" ) ) )

## plot( stateIRF, state = 0, irf.type = "cum.irf", scale = 1, center = FALSE,
##       col = col, lwd = 2, col.ci = col, xlab = xlab,
##       ylab = "Cumulative impulse response by state",
##       main = paste( "State-dependent cumulative impulse-response functions for",
##                     irfVar ),
##       sub = paste( "( MC sample =", stateIRF$nsample, "/ MC", mcStat,
##                    "/ CI signif. =", linearIRF$alpha,
##                    "/ U-test p-val =", signif( stateIRF$cirf.test$p.value, 2 ),
##                    "/ H0: similar states )" ),
##       leg = c( paste( "Low", stateVar, "state" ),
##                paste( "High", stateVar, "state" ) ) )

## plot( stateIRF, state = 0, irf.type = "peak.mult", scale = 1, center = TRUE,
##       col = col, lwd = 2, col.ci = col, xlab = xlab,
##       ylab = "Peak impulse-multiplier by state",
##       main = paste( "State-dependent peak impulse-multiplier functions for", irfVar ),
##       sub = paste( "( MC sample =", stateIRF$nsample, "/ MC", mcStat,
##                    "/ CI signif. =",  linearIRF$alpha,
##                    "/ U-test p-val =", signif( stateIRF$irf.test$p.value, 2 ),
##                    "/ H0: similar states )" ),
##       leg = c( paste( "Low", stateVar, "state" ),
##                paste( "High", stateVar, "state" ) ) )

## plot( stateIRF, state = 0, irf.type = "cum.mult", scale = 1, center = FALSE,
##       col = col, lwd = 2, col.ci = col, xlab = xlab,
##       ylab = "Cumulative impulse-multiplier by state",
##       main = paste( "State-dependent cumulative impulse-multiplier functions for",
##                     irfVar ),
##       sub = paste( "( MC sample =", stateIRF$nsample, "/ MC", mcStat,
##                    "/ CI signif. =", linearIRF$alpha,
##                    "/ U-test p-val =", signif( stateIRF$cirf.test$p.value, 2 ),
##                    "/ H0: similar states )" ),
##       leg = c( paste( "Low", stateVar, "state" ),
##                paste( "High", stateVar, "state" ) ) )


## plot( stateIRF1, state = 0, irf.type = "incr.irf", scale = 1, center = TRUE,
##       col = col, lwd = 2, col.ci = col, xlab = xlab,
##       ylab = "Impulse response by state",
##       main = paste( "State-dependent IRF for", stateIRF1$state ),
##       sub = paste( "( MC sample =", stateIRF1$nsample, "/ MC", mcStat,
##                    "/ CI signif. =",  linearIRF$alpha,
##                    "/ U-test p-val =", signif( stateIRF1$irf.test$p.value, 2 ),
##                    "/ H0: similar states )" ),
##       leg = c( paste( "Low", stateVar, "state" ),
##                paste( "High", stateVar, "state" ) ) )

## plot( stateIRF1, state = 0, irf.type = "cum.irf", scale = 1, center = FALSE,
##       col = col, lwd = 2, col.ci = col, xlab = xlab,
##       ylab = "Cumulative impulse response by state",
##       main = paste( "State-dependent C-IRF for", stateIRF1$state ),
##       sub = paste( "( MC sample =", stateIRF1$nsample, "/ MC", mcStat,
##                    "/ CI signif. =", linearIRF$alpha,
##                    "/ U-test p-val =", signif( stateIRF1$cirf.test$p.value, 2 ),
##                    "/ H0: similar states )" ),
##       leg = c( paste( "Low", stateVar, "state" ),
##                paste( "High", stateVar, "state" ) ) )

## plot( stateIRF1, state = 0, irf.type = "peak.mult", scale = 1, center = TRUE,
##       col = col, lwd = 2, col.ci = col, xlab = xlab,
##       ylab = "Peak impulse-multiplier by state",
##       main = paste( "State-dependent P-IMF for", stateIRF1$state ),
##       sub = paste( "( MC sample =", stateIRF1$nsample, "/ MC", mcStat,
##                    "/ CI signif. =",  linearIRF$alpha,
##                    "/ U-test p-val =", signif( stateIRF1$irf.test$p.value, 2 ),
##                    "/ H0: similar states )" ),
##       leg = c( paste( "Low", stateVar, "state" ),
##                paste( "High", stateVar, "state" ) ) )

## plot( stateIRF1, state = 0, irf.type = "cum.mult", scale = 1, center = FALSE,
##       col = col, lwd = 2, col.ci = col, xlab = xlab,
##       ylab = "Cumulative impulse-multiplier by state",
##       main = paste( "State-dependent C-IMF for", stateIRF1$state ),
##       sub = paste( "( MC sample =", stateIRF1$nsample, "/ MC", mcStat,
##                    "/ CI signif. =", linearIRF$alpha,
##                    "/ U-test p-val =", signif( stateIRF1$cirf.test$p.value, 2 ),
##                    "/ H0: similar states )" ),
##       leg = c( paste( "Low", stateVar, "state" ),
##                paste( "High", stateVar, "state" ) ) )


## plot( stateIRF2, state = 0, irf.type = "incr.irf", scale = 1, center = TRUE,
##       col = col, lwd = 2, col.ci = col, xlab = xlab,
##       ylab = "Impulse response by state",
##       main = paste( "State-dependent IRF for", stateIRF2$state ),
##       sub = paste( "( MC sample =", stateIRF2$nsample, "/ MC", mcStat,
##                    "/ CI signif. =",  linearIRF$alpha,
##                    "/ U-test p-val =", signif( stateIRF2$irf.test$p.value, 2 ),
##                    "/ H0: similar states )" ),
##       leg = c( paste( "Low", stateVar, "state" ),
##                paste( "High", stateVar, "state" ) ) )

## plot( stateIRF2, state = 0, irf.type = "cum.irf", scale = 1, center = FALSE,
##       col = col, lwd = 2, col.ci = col, xlab = xlab,
##       ylab = "Cumulative impulse response by state",
##       main = paste( "State-dependent C-IRF for", stateIRF2$state ),
##       sub = paste( "( MC sample =", stateIRF2$nsample, "/ MC", mcStat,
##                    "/ CI signif. =", linearIRF$alpha,
##                    "/ U-test p-val =", signif( stateIRF2$cirf.test$p.value, 2 ),
##                    "/ H0: similar states )" ),
##       leg = c( paste( "Low", stateVar, "state" ),
##                paste( "High", stateVar, "state" ) ) )

## plot( stateIRF2, state = 0, irf.type = "peak.mult", scale = 1, center = TRUE,
##       col = col, lwd = 2, col.ci = col, xlab = xlab,
##       ylab = "Peak impulse-multiplier by state",
##       main = paste( "State-dependent P-IMF for", stateIRF2$state ),
##       sub = paste( "( MC sample =", stateIRF2$nsample, "/ MC", mcStat,
##                    "/ CI signif. =",  linearIRF$alpha,
##                    "/ U-test p-val =", signif( stateIRF2$irf.test$p.value, 2 ),
##                    "/ H0: similar states )" ),
##       leg = c( paste( "Low", stateVar, "state" ),
##                paste( "High", stateVar, "state" ) ) )

## plot( stateIRF2, state = 0, irf.type = "cum.mult", scale = 1, center = FALSE,
##       col = col, lwd = 2, col.ci = col, xlab = xlab,
##       ylab = "Cumulative impulse-multiplier by state",
##       main = paste( "State-dependent C-IMF for", stateIRF2$state ),
##       sub = paste( "( MC sample =", stateIRF2$nsample, "/ MC", mcStat,
##                    "/ CI signif. =", linearIRF$alpha,
##                    "/ U-test p-val =", signif( stateIRF2$cirf.test$p.value, 2 ),
##                    "/ H0: similar states )" ),
##       leg = c( paste( "Low", stateVar, "state" ),
##                paste( "High", stateVar, "state" ) ) )


## plot( stateIRF3, state = 0, irf.type = "incr.irf", scale = 1, center = TRUE,
##       col = col, lwd = 2, col.ci = col, xlab = xlab,
##       ylab = "Impulse response by state",
##       main = paste( "State-dependent IRF for", stateIRF3$state ),
##       sub = paste( "( MC sample =", stateIRF3$nsample, "/ MC", mcStat,
##                    "/ CI signif. =",  linearIRF$alpha,
##                    "/ U-test p-val =", signif( stateIRF3$irf.test$p.value, 2 ),
##                    "/ H0: similar states )" ),
##       leg = c( paste( "Low", stateVar, "state" ),
##                paste( "High", stateVar, "state" ) ) )

## plot( stateIRF3, state = 0, irf.type = "cum.irf", scale = 1, center = FALSE,
##       col = col, lwd = 2, col.ci = col, xlab = xlab,
##       ylab = "Cumulative impulse response by state",
##       main = paste( "State-dependent C-IRF for", stateIRF3$state ),
##       sub = paste( "( MC sample =", stateIRF3$nsample, "/ MC", mcStat,
##                    "/ CI signif. =", linearIRF$alpha,
##                    "/ U-test p-val =", signif( stateIRF3$cirf.test$p.value, 2 ),
##                    "/ H0: similar states )" ),
##       leg = c( paste( "Low", stateVar, "state" ),
##                paste( "High", stateVar, "state" ) ) )

## plot( stateIRF3, state = 0, irf.type = "peak.mult", scale = 1, center = TRUE,
##       col = col, lwd = 2, col.ci = col, xlab = xlab,
##       ylab = "Peak impulse-multiplier by state",
##       main = paste( "State-dependent P-IMF for", stateIRF3$state ),
##       sub = paste( "( MC sample =", stateIRF3$nsample, "/ MC", mcStat,
##                    "/ CI signif. =",  linearIRF$alpha,
##                    "/ U-test p-val =", signif( stateIRF3$irf.test$p.value, 2 ),
##                    "/ H0: similar states )" ),
##       leg = c( paste( "Low", stateVar, "state" ),
##                paste( "High", stateVar, "state" ) ) )

## plot( stateIRF3, state = 0, irf.type = "cum.mult", scale = 1, center = FALSE,
##       col = col, lwd = 2, col.ci = col, xlab = xlab,
##       ylab = "Cumulative impulse-multiplier by state",
##       main = paste( "State-dependent C-IMF for", stateIRF3$state ),
##       sub = paste( "( MC sample =", stateIRF3$nsample, "/ MC", mcStat,
##                    "/ CI signif. =", linearIRF$alpha,
##                    "/ U-test p-val =", signif( stateIRF3$cirf.test$p.value, 2 ),
##                    "/ H0: similar states )" ),
##       leg = c( paste( "Low", stateVar, "state" ),
##                paste( "High", stateVar, "state" ) ) )


#
# ------ Show detected important states ------
#

rows <- 10

## textplot( format( stateIdent$state.freq[ 1 : rows, ], digits = sDigits ),
##           cmar = 1, show.rownames = FALSE )
## title( main = paste( "Top", rows, "discrete-state frequency (",
##                      stateIdent$var.ref, "C-IRF )" ),
##        sub = paste( "( MC sample =", stateIdent$nsample, "/ Tree depth =",
##                     stateIdent$maxdepth, "/ Significance =",
##                     stateIdent$alpha, "/ Quantiles =",
##                     stateIdent$quantile, " )" ) )


#
# ------ Random forest sensitivity analysis ------
#

## plot( stateSens, xlab = "State-defining variables",
##       main = paste( "Sensitivity analysis of state-defining variables (",
##                     irfVar, "IRF )" ),
##       sub = paste( "( MC sample =", stateSens$nsample, "/ MC", mcStat,
##                    "/ Pseudo R2 =", signif( stateSens$rsq, digits = 2 ),
##                    "/ CI signif. =", stateSens$alpha, ")" ) )


# close plot file
dev.off( )
