#******************************************************************
#
# ----------------- Analysis of the model      ---------------------
#
#******************************************************************

#******************************************************************
#
# ------------ Read Monte Carlo experiment files ----------------
#
#******************************************************************
number_ticks <- function(n) {function(limits) pretty(limits, n)}

folder   <- "./Base_scen"             # data files folder
folderFun<- "./Support_Functions"     # Support functions folder
baseName <- "S"                       # data files base name (same as .lsd file)
nExp     <- 3                         # number of experiments
iniDrop  <- 0                         # initial time steps to drop from analysis (0=none)
nKeep    <- -1                        # number of time steps to keep (-1=all)

expVal <- c( "S1", "S2", "S3" )   # case parameter values

# Aggregate variables to use
logVars <- c( "_I", "_K", "_C", "_G", "_Y", "_A", "_N", "_L",
              "_Profits", "_Tax", "_Inv", "_YN", "_DF", "_AssetsF", "_NI", "_KN", "_P" )
aggrVars <- append( logVars, c( "_FFI", "_UnwHedge", "_UnwSpec", "_UnwPonzi", "_WeiHedge",
                                "_WeiSpec", "_WeiPonzi", "_Lev", "_inf", "_Markup", "_u",
                                "_ProfitRate", "_gA", "_gY", "_LumH", "_LumL",
                                "_MedA", "_MinA", "_PDIndex", "_Minskian", "_HHI" ) )

# Variables to test for stationarity and ergodicity
statErgo.vars <- c( "FFI", "Lev", "u", "Minskian", "UnwHedge", "UnwSpec", "UnwPonzi", "WeiHedge",
                    "WeiSpec", "WeiPonzi" )

# ==== Load functions ====

loadFuns <- paste0( folderFun, '/', list.files( folderFun ) )
for( i in 1:length( loadFuns ) ) source( loadFuns[i], local = T )

# ==== Process LSD result files ====

# Package with LSD interface functions
library( LSDinterface, verbose = FALSE, quietly = TRUE )
library( dplyr, verbose = FALSE, quietly = TRUE )

# remove warnings for saved data
# !diagnostics suppress = A, S, M, m

# ---- Read data files ----

# Function to read one experiment data (to be parallelized)
readExp <- function( exper ) {
  if( nExp > 1 ) {
    myFiles <- list.files( path = folder, pattern = paste0( baseName, exper, "_[0-9]+.res" ),
                           full.names = TRUE )
  } else {
    myFiles <- list.files( path = folder, pattern = paste0( baseName, "_[0-9]+.res" ),
                           full.names = TRUE )
  }

  if( length( myFiles ) < 1 )
    stop( "Data files not found. Check 'folder', 'baseName' and 'nExp' parameters." )

  cat( "Data files: ", myFiles, "\n" )

  # Read data from text files and format it as a 3D array with labels
  mc <- read.3d.lsd( myFiles, aggrVars, skip = iniDrop, nrows = nKeep, nnodes = 1 )

  # Get dimensions information
  nTsteps <- dim( mc )[ 1 ]              # number of time steps
  nVar <- dim( mc )[ 2 ]                 # number of variables
  nSize  <- dim( mc )[ 3 ]               # Monte Carlo sample size

  # Compute Monte Carlo averages and std. deviation and store in 2D arrrays
  stats <- info.stats.lsd( mc )

  # Insert a t column
  t <- as.integer( rownames( stats$avg ) )
  A <- as.data.frame( cbind( t, stats$avg ) )
  S <- as.data.frame( cbind( t, stats$sd ) )
  M <- as.data.frame( cbind( t, stats$max ) )
  m <- as.data.frame( cbind( t, stats$min ) )

  # Save temporary results to disk to save memory
  tmpFile <- paste0( folder, "/", baseName, exper, "_aggr.Rdata" )
  save( mc, A, S, M, m, nTsteps, nVar, nSize, file = tmpFile )

  return( tmpFile )
}

# load each experiment serially
tmpFiles <- lapply( 1 : nExp, readExp )

# ---- Organize data read from files ----

# fill the lists to hold data
Adata <- list()  # average data
Sdata <- list()  # standard deviation data
Mdata <- list()  # maximum data
mdata <- list()  # minimum data
nTsteps.1 <- nSize.1 <- 0

for( k in 1 : nExp ) {                      # realocate data in separate lists

  load( tmpFiles[[ k ]] )                   # pick data from disk
  file.remove( tmpFiles[[ k ]] )            # and delete temporary file

  if( k > 1 && ( nTsteps != nTsteps.1 || nSize != nSize.1 ) )
    stop( "Inconsistent data files.\nSame number of time steps and of MC runs is required." )
  
  for( i in 1 : dim(mc)[3] )
  {
    indices <- data.frame( exp_n = rep( expVal[k], dim(mc)[1] ),
                mc_n = rep( paste0( 'mc_', i ), dim(mc)[1] ),
                t = A$t )
    index   <- data.frame( exp_n = rep( expVal[k], dim(mc)[1] ) )
     
    if( !exists( 'mcData' ) )
    {
      mcData <- cbind( indices, mc[ , , i ] )
    }
    else
    {
      tmpData <- cbind( indices, mc[ ,  , i ] )
      mcData  <- rbind( mcData, tmpData )
    }
    
  }
  rm( mc )
  Adata[[ k ]] <- cbind( index, A )
  Sdata[[ k ]] <- cbind( index, S )
  Mdata[[ k ]] <- cbind( index, M )
  mdata[[ k ]] <- cbind( index, m )
  nTsteps.1 <- nTsteps
  nSize.1 <- nSize
}


# free memory
rm( tmpFiles, tmpData, index, indices, A, S, M, m, nTsteps.1, nSize.1 )
invisible( gc( verbose = FALSE ) )

# reorganize data into data frames

Adata <- do.call( 'rbind', Adata )
mdata <- do.call( 'rbind', mdata )
Sdata <- do.call( 'rbind', Sdata )
Mdata <- do.call( 'rbind', Mdata )

colnames(mcData)<- stringr::str_remove( string = colnames(mcData), "X_" )
colnames(Adata) <- stringr::str_remove( string = colnames(Adata), "X_" )
colnames(mdata) <- stringr::str_remove( string = colnames(mdata), "X_" )
colnames(Sdata) <- stringr::str_remove( string = colnames(Sdata), "X_" )
colnames(Mdata) <- stringr::str_remove( string = colnames(Mdata), "X_" )



#******************************************************************
#
# --------------------- Plot statistics -------------------------
#
#******************************************************************

# ===================== User parameters =========================

bCase     <- 1      # experiment to be used as base case
CI        <- 0.95   # desired confidence interval
warmUpPlot<- 100    # number of "warm-up" runs for plots
nTplot    <- -1     # last period to consider for plots (-1=all)
warmUpStat<- 300    # warm-up runs to evaluate all statistics
nTstat    <- -1     # last period to consider for statistics (-1=all)
lowP      <- 6      # bandpass filter minimum period
highP     <- 32     # bandpass filter maximum period
bpfK      <- 12     # bandpass filter order
lags      <- 4      # lags to analyze
bPlotCoef <- 1.5    # boxplot whiskers extension from the box (0=extremes)
bPlotNotc <- FALSE  # use boxplot notches
smoothing <- 1e5    # HP filter smoothing factor (lambda)
tMax      <- 500    # maximum period in plots 
tMin      <- 100    # minimum period in plots

repName   <- ""     # report files base name (if "" same baseName)
sDigits   <- 4      # significant digits in tables
plotAxis  <- 16     # font size of axis in plots
plotLegend<- 14  	  # font size of legends in plots
plotW     <- 30     # plot window width
plotH     <- 15     # plot window height

# Colors assigned to each experiment's lines in graphics
colors <- c( "black", "blue", "red", "orange", "green", "brown" )

# Line types assigned to each experiment
lTypes <- c( "solid", "solid", "solid", "solid", "solid", "solid" )

# Point types assigned to each experiment
pTypes <- c( 4, 4, 4, 4, 4, 4 )

# ====== Save aggregate data needed for micro-macro analysis =====================
exportData <- mcData %>% select( exp_n, mc_n, t, Lev, PDIndex )
tmpFile    <- paste0( folder, "/mcData_aggr.Rdata" )
save( exportData, file = tmpFile )
rm( exportData, tmpFile )

# ===================== Cyclical components of select series =====================
library( mFilter )
library( dplyr )
library( ggplot2 )
library( tidyr )

filter_series_cycle <- function( x, drift = T )
  {bkfilter(x, pl = lowP, pu = highP, type = c( "fixed" ), nfix = bpfK, drift = drift)}

cycle_demand <- Adata %>% 
  select( exp_n, t, Y, C, I, G ) %>%
  group_by( exp_n ) %>%
  mutate( Y = filter_series_cycle( log( Y ) )$cycle,
          C = filter_series_cycle( log( C ) )$cycle,
          I = filter_series_cycle( log( I ) )$cycle,
          G = filter_series_cycle( log( G ) )$cycle,
        ) %>% 
  filter( t <= tMax, t >= tMin ) %>%
  mutate( t = t - min( t ) )
           
cases <- unique( cycle_demand$exp_n )
n <- length( cases )                  
for( i in 1:n ){
  subsetData <- cycle_demand %>% ungroup() %>%
    filter( exp_n == cases[i] ) %>%
    select( -exp_n )
  colnames(subsetData)<- c('Time', 'GDP', 'Consumption', 'Investment', 'Gov. Exp.' )
  subsetData <- subsetData %>% 
    pivot_longer( cols = c( GDP, Consumption, Investment, `Gov. Exp.` ) ) %>%
    mutate( name = factor( name, levels = c( 'GDP', 'Consumption', 'Investment', 'Gov. Exp.' ) ) )
  
  ggplot( subsetData, aes(x = Time, y = value, group = name, linetype = name, color = name) ) + 
    geom_line(lwd = 0.7) + 
    xlab('Time') + ylab( 'Bandpass filtered series' ) +
    #theme_bw() +   
    theme(legend.title=element_blank(), axis.text = element_text(size = plotAxis), 
                         legend.text=element_text(size= plotLegend), strip.text.y = element_text(size = plotAxis, angle = 90),
                         legend.key.size = unit(1, "cm"), legend.key = element_blank(), legend.position='bottom',
                         legend.justification="center",
                         legend.box.margin=margin(-10,-10,-10,-10),
                         axis.title=element_text(size=plotLegend,face="bold")) +
    scale_colour_grey() +
    scale_x_continuous(expand = c(0.02,0)) +
    scale_y_continuous(expand=c(0.04,0))  
    ggsave( paste0( './Output/cycleDemand_', cases[i], '.pdf' ) , width = plotW, height = plotH, units = "cm")                                  
}
rm( cycle_demand )

# ===================== GDP series in logs =====================
level_series <- Adata %>% 
  select( exp_n, t, Y, C, I, G ) %>%
  mutate( Y = log( Y ), C = log( C ), I = log( I ), G = log( G )) %>%
  filter( t <= tMax, t >= tMin ) %>%
  mutate( t = t - min( t ) )

for( i in 1:n ){
  subsetData <- level_series %>% ungroup() %>%
    filter( exp_n == cases[i] ) %>%
    select( -exp_n )
  colnames(subsetData)<- c( 'Time', 'GDP', 'Consumption', 'Investment', 'Gov. Exp.' )
  subsetData <- subsetData %>% 
    pivot_longer( cols = c( GDP, Consumption, Investment, `Gov. Exp.` ) ) %>%
    mutate( name = factor( name, levels = c( 'GDP', 'Consumption', 'Investment', 'Gov. Exp.' ) ) )
  
  ggplot( subsetData, aes(x = Time, y = value, group = name, linetype = name, color = name) ) + 
    geom_line(lwd = 0.7) + 
    xlab('Time') + ylab( 'Logs' ) +
    #theme_bw() +   
    theme(legend.title=element_blank(), axis.text = element_text(size = plotAxis), 
                         legend.text=element_text(size= plotLegend), strip.text.y = element_text(size = plotAxis, angle = 90),
                         legend.key.size = unit(1, "cm"), legend.key = element_blank(), legend.position='bottom',
                         legend.justification="center",
                         legend.box.margin=margin(-10,-10,-10,-10),
                         axis.title=element_text(size=plotLegend,face="bold")) +
    scale_colour_grey() +
    scale_x_continuous(expand = c(0.02,0)) +
    scale_y_continuous(expand=c(0.04,0))  
    ggsave( paste0( './Output/levelDemand_', cases[i], '.pdf' ) , width = plotW, height = plotH, units = "cm")                                  
}                              

rm(level_series)

# ===================== Capacity utilisation =====================
capacity_u <- Adata %>% 
  select( exp_n, t, u ) %>%
  group_by( exp_n ) %>%
  mutate( trend_u = filter_series_cycle( u, drift = F )$trend ) %>%
  filter( t <= tMax, t >= tMin ) %>%
  mutate( t = t - min( t ) )

capacity_u_sd <- Sdata %>% 
  select( exp_n, t, u ) %>%
  filter( t <= tMax, t >= tMin ) %>%
  mutate( t = t - min( t ) )
capacity_u_sd$u_max <- capacity_u$u + qnorm( 1 - ( 1 - CI ) / 2 ) * capacity_u_sd$u / sqrt( nSize )
capacity_u_sd$u_min <- capacity_u$u - qnorm( 1 - ( 1 - CI ) / 2 ) * capacity_u_sd$u / sqrt( nSize )

for( i in 1:n ){
  subsetData <- capacity_u %>% ungroup() %>%
    filter( exp_n == cases[i] ) %>%
    select( -exp_n )
  colnames(subsetData)<- c( 'Time', 'u', 'trend' )
  subsetData <- subsetData %>% 
    pivot_longer( cols = c( u, trend ) ) %>%
    mutate( name = factor( name, levels = c( 'u', 'trend' ) ) )
  
  ggplot( subsetData, aes(x = Time, y = value, group = name, linetype = name, color = name) ) + 
    geom_line( show.legend=F ) + 
    xlab('Time') + ylab( 'Capacity utilisation (95% CI) and trend component' ) +
    #theme_bw() +  
    theme(legend.title=element_blank(), axis.text = element_text(size = plotAxis), 
                         legend.text=element_text(size= plotLegend), strip.text.y = element_text(size = plotAxis, angle = 90),
                         legend.key.size = unit(1, "cm"), legend.key = element_blank(), legend.position='bottom',
                         legend.justification="center",
                         legend.box.margin=margin(-10,-10,-10,-10),
                         axis.title=element_text(size=plotLegend,face="bold")) +
    scale_colour_manual(values = c('black', 'black')) +
    scale_size_manual(values = c(0.4, 1.2)) +
    scale_linetype_manual(values = c('solid', 'dashed')) +
    scale_x_continuous(expand = c(0.02,0)) +
    scale_y_continuous(expand=c(0.04,0))  
    ggsave( paste0( './Output/capacityUtilisation_', cases[i], '.pdf' ) , width = plotW, height = plotH, units = "cm")                                  
}                              

rm( capacity_u )


# ===================== Bandpass filtered series =====================

# Subset series to be filtered
bpf_series <- mcData %>% 
  select( exp_n, mc_n, t, Y, I, K, u, N, inf, Markup, A, Inv, L, Lev, DF, FFI, Profits, AssetsF ) %>%
  mutate( LiqRatio = DF / AssetsF, NLev = ( L - DF ) / AssetsF  ) %>%
  select( -AssetsF ) %>%
  group_by( exp_n, mc_n ) %>%
  mutate( chg_Inv = Inv - lag( Inv ) ) %>%
  mutate( chg_Inv = ifelse( is.na(chg_Inv), 0, chg_Inv ) ) %>%
  mutate( GDP_cycle = filter_series_cycle( log( Y ) )$cycle,
          I_cycle = filter_series_cycle( log( I ) )$cycle,
          K_cycle = filter_series_cycle( log( K ) )$cycle,  
          u_cycle = filter_series_cycle( u, drift = F )$cycle,
          N_cycle = filter_series_cycle( log( N ) )$cycle,
          inf_cycle = filter_series_cycle( inf , drift = F )$cycle,
          Markup_cycle = filter_series_cycle( Markup , drift = F )$cycle,
          A_cycle = filter_series_cycle( log( A ) )$cycle, 
          Inv_cycle = filter_series_cycle( chg_Inv )$cycle, 
          L_cycle = filter_series_cycle( log( L ) )$cycle,
          Lev_cycle = filter_series_cycle( Lev , drift = F )$cycle,
          DF_cycle = filter_series_cycle( log( DF ) )$cycle,
          LiqRatio_cycle = filter_series_cycle( LiqRatio, drift = F )$cycle,
          NetLev_cycle = filter_series_cycle( NLev, drift = F )$cycle,
          FFI_cycle = filter_series_cycle( FFI, drift = F )$cycle,
          Profits_cycle = filter_series_cycle( log( Profits ) )$cycle ) %>%
  select( exp_n, mc_n, t, contains( '_cycle' ) ) %>%
  filter( t <= tMax, t >= tMin )

# Do the cross-correlations
ids <- unique(bpf_series$mc_n)
exps<- unique(bpf_series$exp_n)

if(exists('crossCorrels')){rm(crossCorrels)}

for( j in 1:length(exps) ){
  for( i in 1:length(ids) ){
    rows <- which( bpf_series$mc_n == ids[i] & bpf_series$exp_n == exps[j] )
    x    <- apply( bpf_series[rows, !colnames(bpf_series) %in% c( 'exp_n', 'mc_n', 't') ], 2, 
                  function(x) ccf(bpf_series$GDP_cycle[rows], x, na.action = na.pass, plot = F, lag.max = 4)$acf)
    x    <- cbind( exp_n = unique( bpf_series$exp_n )[j],
                   mc_n = rep( ids[i], nrow(x) ), 
                   Lag = paste('Lag', seq(-4, 4)),  x )
    if( !exists('crossCorrels') ){
      crossCorrels <- x
    } else {
      crossCorrels <- rbind(crossCorrels, x)
    }
  }
}

rm(x, rows, ids)

crossCorrels <- as.data.frame(crossCorrels)

mean_modified <- function(x) mean(as.numeric(x), na.rm = T)
sd_modified <- function(x) sd(as.numeric(x), na.rm = T)
roundSd <- function(x, digits) paste0('(', sprintf("%.4f", round( x, digits )), ')')

meanCCorrel <- crossCorrels %>% mutate( Type = 'Mean' ) %>% group_by( exp_n, Lag, Type ) %>% 
  select( -mc_n ) %>% summarise_all( mean_modified ) %>%
  mutate( Lag = factor( Lag, levels = c('Lag -4', 'Lag -3', 'Lag -2', 'Lag -1',
                                        'Lag 0', 'Lag 1', 'Lag 2', 'Lag 3', 'Lag 4') ) ) %>%
  arrange( exp_n, Lag ) %>% mutate_if( is.numeric, round, 3 ) %>% mutate_all( as.character )
sdCrossCor  <- crossCorrels %>% mutate( Type = 's.d.' ) %>% group_by( exp_n, Lag, Type ) %>% 
  select( -mc_n ) %>% summarise_all( sd_modified ) %>%
  mutate( Lag = factor( Lag, levels = c('Lag -5', 'Lag -4', 'Lag -3', 'Lag -2', 'Lag -1',
                                        'Lag 0', 'Lag 1', 'Lag 2', 'Lag 3', 'Lag 4') ) ) %>%
  arrange( exp_n, Lag ) %>% mutate_if( is.numeric, roundSd, 4 )

table1CCf    <- rbind( meanCCorrel, sdCrossCor )
colnames(table1CCf) <- c( 'exp_n', 'Lag', 'Type', '01Output', '02Investment',
                          '03Capital Stock', '04Capacity Utilisation', '05Employment', 
                          '06Inflation', '07Markup', '08Productivity', '09Chg. Inventories', 
                          '10Debt', '11Leverage', '12Firms deposits', '13Firms liquidity ratio',
                          '14Net leverage', '15Financial fragitily index', '16Aggregate profits')

table1CCf    <- table1CCf %>% arrange( exp_n, Lag )
table1CCf <- table1CCf %>% 
  pivot_longer( cols = colnames(table1CCf)[!colnames(table1CCf) %in% c( 'exp_n', 'Lag', 'Type' )] ) %>%
  pivot_wider( names_from = 'Lag' ) %>% arrange( exp_n, name, Type ) %>%
  mutate( name = substring(name, 3) ) %>%
  mutate( name = ifelse( Type == 's.d.', "", name ) ) %>%
  select(-Type)

for( i in 1:length(exps) ) {
  tmpVar <- exps[i]
  ptable <- subset( table1CCf, exp_n == tmpVar ) %>% ungroup() %>% select(-exp_n)
  print(xtable::xtable( ptable, type = "latex"), 
        include.rownames=FALSE, file = paste0( "./Output/crossCorel_", tmpVar, '.tex' ))
}

rm( meanCCorrel, exps, sdCrossCor, subsetData, tmpVar, ptable, bpf_series, table1CCf )

# ===================== Plot financial statuses (unweighted) =====================
fin_status <- Adata %>% select( exp_n, t, UnwHedge, UnwSpec, UnwPonzi ) %>%
  filter( t <= tMax, t >= tMin ) %>%
  mutate( t = t - min( t ) )

fin_status_sd <- Sdata %>% select( exp_n, t, UnwHedge, UnwSpec, UnwPonzi ) %>%
  filter( t <= tMax, t >= tMin ) %>%
  mutate( t = t - min( t ) )

fin_status_sd$UnwHedge_max <- fin_status$UnwHedge + qnorm( 1 - ( 1 - CI ) / 2 ) * fin_status_sd$UnwHedge / sqrt( nSize )
fin_status_sd$UnwHedge_min <- fin_status$UnwHedge - qnorm( 1 - ( 1 - CI ) / 2 ) * fin_status_sd$UnwHedge / sqrt( nSize )
fin_status_sd$UnwSpec_max  <- fin_status$UnwSpec + qnorm( 1 - ( 1 - CI ) / 2 ) * fin_status_sd$UnwSpec / sqrt( nSize )
fin_status_sd$UnwSpec_min  <- fin_status$UnwSpec - qnorm( 1 - ( 1 - CI ) / 2 ) * fin_status_sd$UnwSpec / sqrt( nSize )
fin_status_sd$UnwPonzi_max <- fin_status$UnwPonzi + qnorm( 1 - ( 1 - CI ) / 2 ) * fin_status_sd$UnwPonzi / sqrt( nSize )
fin_status_sd$UnwPonzi_min <- fin_status$UnwPonzi - qnorm( 1 - ( 1 - CI ) / 2 ) * fin_status_sd$UnwPonzi / sqrt( nSize )

bottom <- fin_status_sd %>% select( exp_n, t, UnwHedge_min, UnwSpec_min, UnwPonzi_min ) %>%
  pivot_longer( cols = c('UnwHedge_min', 'UnwSpec_min', 'UnwPonzi_min' ), values_to = 'Bottom') %>%
  mutate( name = ifelse( name == 'UnwHedge_min', 'Hedge', ifelse( name == 'UnwSpec_min', 'Speculative', 'Ponzi' ) ) )
top <- fin_status_sd %>% select( exp_n, t, UnwHedge_max, UnwSpec_max, UnwPonzi_max ) %>%
  pivot_longer( cols = c('UnwHedge_max', 'UnwSpec_max', 'UnwPonzi_max' ), values_to = 'Top') %>%
  mutate( name = ifelse( name == 'UnwHedge_max', 'Hedge', ifelse( name == 'UnwSpec_max', 'Speculative', 'Ponzi' ) ) )

colnames(fin_status)<- c( 'exp_n', 'Time', 'Hedge', 'Speculative', 'Ponzi' )
fin_status <- fin_status %>% 
  pivot_longer( cols = c( Hedge, Speculative, Ponzi ) )

fin_status$Bottom <- bottom$Bottom
fin_status$Top    <- top$Top

for( i in 1:n ){
  subsetData <- fin_status %>% ungroup() %>%
    filter( exp_n == cases[i] ) %>%
    select( -exp_n )
  
  ggplot( subsetData, aes(x = Time, y = value, group = name, linetype = name, color = name) ) + 
    geom_line(  ) + 
    geom_ribbon( aes( ymin = Bottom, ymax = Top ), alpha = 0.2, colour = NA ) +
    xlab('Time') + ylab('Frequency (% of total)') +
    #theme_bw() +   
    theme(legend.title=element_blank(), axis.text = element_text(size = plotAxis), 
                         legend.text=element_text(size= plotLegend), strip.text.y = element_text(size = plotAxis, angle = 90),
                         legend.key.size = unit(1, "cm"), legend.key = element_blank(), legend.position='bottom',
                         legend.justification="center",
                         legend.box.margin=margin(-10,-10,-10,-10),
                         axis.title=element_text(size=plotLegend,face="bold")) +
    scale_color_manual(values=c("black", "blue", "red")) +
    scale_x_continuous(expand = c(0.02,0)) +
    scale_y_continuous(expand=c(0.04,0))  
    ggsave( paste0( './Output/financingRegimesUNW_', cases[i], '.pdf' ) , width = 20, height = 16, units = "cm")                                  
}                              

rm( fin_status, fin_status_sd, bottom, top, subsetData )

# ===================== Plot financial statuses (weighted) =====================
fin_status <- Adata %>% select( exp_n, t, WeiHedge, WeiSpec, WeiPonzi ) %>%
  filter( t <= tMax, t >= tMin ) %>%
  mutate( t = t - min( t ) )

fin_status_sd <- Sdata %>% select( exp_n, t, WeiHedge, WeiSpec, WeiPonzi ) %>%
  filter( t <= tMax, t >= tMin ) %>%
  mutate( t = t - min( t ) )

fin_status_sd$WeiHedge_max <- fin_status$WeiHedge + qnorm( 1 - ( 1 - CI ) / 2 ) * fin_status_sd$WeiHedge / sqrt( nSize )
fin_status_sd$WeiHedge_min <- fin_status$WeiHedge - qnorm( 1 - ( 1 - CI ) / 2 ) * fin_status_sd$WeiHedge / sqrt( nSize )
fin_status_sd$WeiSpec_max  <- fin_status$WeiSpec + qnorm( 1 - ( 1 - CI ) / 2 ) * fin_status_sd$WeiSpec / sqrt( nSize )
fin_status_sd$WeiSpec_min  <- fin_status$WeiSpec - qnorm( 1 - ( 1 - CI ) / 2 ) * fin_status_sd$WeiSpec / sqrt( nSize )
fin_status_sd$WeiPonzi_max <- fin_status$WeiPonzi + qnorm( 1 - ( 1 - CI ) / 2 ) * fin_status_sd$WeiPonzi / sqrt( nSize )
fin_status_sd$WeiPonzi_min <- fin_status$WeiPonzi - qnorm( 1 - ( 1 - CI ) / 2 ) * fin_status_sd$WeiPonzi / sqrt( nSize )

bottom <- fin_status_sd %>% select( exp_n, t, WeiHedge_min, WeiSpec_min, WeiPonzi_min ) %>%
  pivot_longer( cols = c('WeiHedge_min', 'WeiSpec_min', 'WeiPonzi_min' ), values_to = 'Bottom') %>%
  mutate( name = ifelse( name == 'WeiHedge_min', 'Hedge', ifelse( name == 'WeiSpec_min', 'Speculative', 'Ponzi' ) ) )
top <- fin_status_sd %>% select( exp_n, t, WeiHedge_max, WeiSpec_max, WeiPonzi_max ) %>%
  pivot_longer( cols = c('WeiHedge_max', 'WeiSpec_max', 'WeiPonzi_max' ), values_to = 'Top') %>%
  mutate( name = ifelse( name == 'WeiHedge_max', 'Hedge', ifelse( name == 'WeiSpec_max', 'Speculative', 'Ponzi' ) ) )

colnames(fin_status)<- c( 'exp_n', 'Time', 'Hedge', 'Speculative', 'Ponzi' )
fin_status <- fin_status %>% 
  pivot_longer( cols = c( Hedge, Speculative, Ponzi ) )

fin_status$Bottom <- bottom$Bottom
fin_status$Top    <- top$Top

for( i in 1:n ){
  subsetData <- fin_status %>% ungroup() %>%
    filter( exp_n == cases[i] ) %>%
    select( -exp_n )
  
  ggplot( subsetData, aes(x = Time, y = value, group = name, linetype = name, color = name) ) + 
    geom_line(  ) + 
    geom_ribbon( aes( ymin = Bottom, ymax = Top ), alpha = 0.2, colour = NA ) +
    xlab('Time') + ylab('Frequency (% of total)') +
    #theme_bw() +  
    theme(legend.title=element_blank(), axis.text = element_text(size = plotAxis), 
                         legend.text=element_text(size= plotLegend), strip.text.y = element_text(size = plotAxis, angle = 90),
                         legend.key.size = unit(1, "cm"), legend.key = element_blank(), legend.position='bottom',
                         legend.justification="center",
                         legend.box.margin=margin(-10,-10,-10,-10),
                         axis.title=element_text(size=plotLegend,face="bold")) +
    scale_color_manual(values=c("black", "blue", "red")) +
    scale_x_continuous(expand = c(0.02,0)) +
    scale_y_continuous(expand=c(0.04,0))  
    ggsave( paste0( './Output/financingRegimesWeighted_', cases[i], '.pdf' ) , width = 20, height = 16, units = "cm")                                  
}                              

rm( fin_status, fin_status_sd, bottom, top, subsetData )


# ===================== Plot difference technology =====================
dist_productivity <- Adata %>% select( exp_n, t, MedA, MinA ) %>%
  filter( t <= tMax, t >= tMin ) %>%
  mutate( t = t - min( t ) )

dist_productivity_sd <- Sdata %>% select( exp_n, t, MedA, MinA ) %>%
  filter( t <= tMax, t >= tMin ) %>%
  mutate( t = t - min( t ) )
dist_productivity_sd$MedA_max <- dist_productivity$MedA + qnorm( 1 - ( 1 - CI ) / 2 ) * dist_productivity_sd$MedA / sqrt( nSize )
dist_productivity_sd$MedA_min <- dist_productivity$MedA - qnorm( 1 - ( 1 - CI ) / 2 ) * dist_productivity_sd$MedA / sqrt( nSize )
dist_productivity_sd$MinA_max <- dist_productivity$MinA + qnorm( 1 - ( 1 - CI ) / 2 ) * dist_productivity_sd$MinA / sqrt( nSize )
dist_productivity_sd$MinA_min <- dist_productivity$MinA - qnorm( 1 - ( 1 - CI ) / 2 ) * dist_productivity_sd$MinA / sqrt( nSize )
bottom <- dist_productivity_sd %>% select( -MedA, -MinA,  -MedA_max, -MinA_max ) %>%
  pivot_longer( cols = c('MedA_min', 'MinA_min'), values_to = 'Bottom') %>%
  mutate( name = ifelse( name == 'MedA_min', 'Median', 'Minimum' ) )
top <- dist_productivity_sd %>% select( -MedA, -MinA,  -MedA_min, -MinA_min ) %>%
  pivot_longer( cols = c('MedA_max', 'MinA_max'), values_to = 'Top') %>% 
  mutate( name = ifelse( name == 'MedA_min', 'Median', 'Minimum' ) )

colnames(dist_productivity)<- c( 'exp_n', 'Time', 'Median', 'Minimum' )
dist_productivity <- dist_productivity %>% 
  pivot_longer( cols = c( Median, Minimum ) )

dist_productivity$Bottom <- bottom$Bottom
dist_productivity$Top    <- top$Top

for( i in 1:n ){
  subsetData <- dist_productivity %>% ungroup() %>%
    filter( exp_n == cases[i] ) %>%
    select( -exp_n )

  ggplot( subsetData, aes(x = Time, y = value, group = name, linetype = name, color = name) ) + 
    geom_line(  ) + 
    geom_ribbon( aes( ymin = Bottom, ymax = Top ), alpha = 0.2, colour = NA ) +
    xlab('Time') + ylab( 'Labour productivity vs most productive firm' ) +
    #theme_bw() +   
    theme(legend.title=element_blank(), axis.text = element_text(size = plotAxis), 
                         legend.text=element_text(size= plotLegend), strip.text.y = element_text(size = plotAxis, angle = 90),
                         legend.key.size = unit(1, "cm"), legend.key = element_blank(), legend.position='bottom',
                         legend.justification="center",
                         legend.box.margin=margin(-10,-10,-10,-10),
                         axis.title=element_text(size=plotLegend,face="bold")) +
    scale_color_manual(values=c("black", "gray20", "gray50")) +
    scale_x_continuous(expand = c(0.02,0)) +
    scale_y_continuous(expand=c(0.04,0))  
    ggsave( paste0( './Output/distprodctivity_', cases[i], '.pdf' ) , width = 20, height = 16, units = "cm")                                  
}                              

# ===================== Plot lumpiness of investment =====================
lumpiness <- Adata %>% select( exp_n, t, LumH, LumL ) %>%
  filter( t <= tMax, t >= tMin ) %>%
  mutate( t = t - min( t ) )

lumpiness_sd <- Sdata %>% select( exp_n, t, LumH, LumL ) %>%
  filter( t <= tMax, t >= tMin ) %>%
  mutate( t = t - min( t ) )
lumpiness_sd$High_max <- lumpiness$LumH + qnorm( 1 - ( 1 - CI ) / 2 ) * lumpiness_sd$LumH / sqrt( nSize )
lumpiness_sd$High_min <- lumpiness$LumH - qnorm( 1 - ( 1 - CI ) / 2 ) * lumpiness_sd$LumH / sqrt( nSize )
lumpiness_sd$Low_max  <- lumpiness$LumL + qnorm( 1 - ( 1 - CI ) / 2 ) * lumpiness_sd$LumL / sqrt( nSize )
lumpiness_sd$Low_min  <- lumpiness$LumL - qnorm( 1 - ( 1 - CI ) / 2 ) * lumpiness_sd$LumL / sqrt( nSize )
bottom <- lumpiness_sd %>% select( -LumH, -LumL,  -High_max, -Low_max ) %>%
  pivot_longer( cols = c('High_min', 'Low_min'), values_to = 'Bottom') %>%
  mutate( name = ifelse( name == 'High_min', 'I/K>=0.2', 'I/K<0.02' ) )
top <- lumpiness_sd %>% select( -LumH, -LumL,  -High_min, -Low_min ) %>%
  pivot_longer( cols = c('High_max', 'Low_max'), values_to = 'Top') %>% 
  mutate( name = ifelse( name == 'High_max', 'I/K>=0.2', 'I/K<0.02'  ) )

colnames(lumpiness)<- c( 'exp_n', 'Time', 'I/K>=0.2', 'I/K<0.02'  )
lumpiness <- lumpiness %>% 
  pivot_longer( cols = c( `I/K>=0.2`, `I/K<0.02` ) )

lumpiness$Bottom <- bottom$Bottom
lumpiness$Top    <- top$Top

for( i in 1:n ){
  subsetData <- lumpiness %>% ungroup() %>%
    filter( exp_n == cases[i] ) %>%
    select( -exp_n )
  
  ggplot( subsetData, aes(x = Time, y = value, group = name, linetype = name, color = name) ) + 
    geom_line(  ) + 
    geom_ribbon( aes( ymin = Bottom, ymax = Top ), alpha = 0.2, colour = NA ) +
    xlab('Time') + ylab( 'Percent (weighted by size)' ) +
    #theme_bw() +   
    theme(legend.title=element_blank(), axis.text = element_text(size = plotAxis), 
                         legend.text=element_text(size= plotLegend), strip.text.y = element_text(size = plotAxis, angle = 90),
                         legend.key.size = unit(1, "cm"), legend.key = element_blank(), legend.position='bottom',
                         legend.justification="center",
                         legend.box.margin=margin(-10,-10,-10,-10),
                         axis.title=element_text(size=plotLegend,face="bold")) +
    scale_color_manual(values=c("black", "gray20", "gray50")) +
    scale_x_continuous(expand = c(0.02,0)) +
    scale_y_continuous(expand=c(0.04,0))  
    ggsave( paste0( './Output/lumpiness_', cases[i], '.pdf' ) , width = 20, height = 16, units = "cm")                                  
}                              


# ===================== Regression and regression plots =====================
require(sandwich)
require(lmtest)
require(effects)

fragReg <- mcData %>% select( exp_n, mc_n, t, FFI, Lev, PDIndex, ProfitRate, I, P, KN ) %>%
  group_by( exp_n, mc_n ) %>% 
  mutate( llev = lag( Lev ), invRate = P * I / lag( KN ) ) %>%
  select( -I, -P, -KN ) %>%
  filter( t <= tMax, t >= tMin ) %>%
  mutate( id = as.factor( mc_n ) )
  
for( i in 1:n ){
  subsetData <- fragReg %>% ungroup() %>%
    filter( exp_n == cases[i] ) %>%
    select( -exp_n )
  
  frag    <- lm( FFI ~ 1 + llev + ProfitRate + invRate + PDIndex + id, data = subsetData )
  fragRob <- coeftest(frag, vcov. = NeweyWest)

  # generate table
  if( !exists( 'table_coeffs' ) ){
    table_coeffs <- data.frame( Variable = c(rbind( c( 'Leverage', 'Profit Rate', 'Investment Ratio', 'PDIndex' ), rep( '', 4 ) ) ),
                                Experiment = c(rbind( fragRob[2:5, 1], fragRob[2:5, 2] ) )
                              )
    colnames( table_coeffs )[2] <- cases[i]
  }
  else{
    table_coeffs$Coefficient <- c(rbind( fragRob[2:5, 1], fragRob[2:5, 2] ) )
    colnames( table_coeffs )[ ncol(table_coeffs) ] <- cases[i]
  }
    
  vcovNWFrag  <- NeweyWest(frag)
  vcovNWFrag2 <- function(mod = mod, ...) return(vcovNWFrag)
  
  
  eff1 <- effect(term=c("llev"),mod=frag, vcov. = vcovNWFrag2, partial.residuals = F, default.levels = 20)
  eff2 <- effect(term=c("ProfitRate"),mod=frag, vcov. = vcovNWFrag2, partial.residuals = F, default.levels = 20)
  eff3 <- effect(term=c("invRate"),mod=frag, vcov. = vcovNWFrag2, partial.residuals = F, default.levels = 20)
  eff4 <- effect(term=c("PDIndex"),mod=frag, vcov. = vcovNWFrag2, partial.residuals = F, default.levels = 20)
  
  eff1 <- with( eff1, data.frame( value = eff1$variable$llev[3]$levels, var = '(a) Leverage',
                fit = fit, upper = upper, lower = lower, exp_n = cases[i] ) )
  eff2 <- with( eff2, data.frame( value = eff2$variable$ProfitRate[3]$levels, var = '(c) Profit rate',
                fit = fit, upper = upper, lower = lower, exp_n = cases[i] ) )
  eff3 <- with( eff3, data.frame( value = eff3$variable$invRate[3]$levels, var = '(b) Investment ratio',
                                  fit = fit, upper = upper, lower = lower, exp_n = cases[i] ) )
  eff4 <- with( eff4, data.frame( value = eff4$variable$PDIndex[3]$levels, var = '(d) PDDI',
                                  fit = fit, upper = upper, lower = lower, exp_n = cases[i] ) )
    
  ggplot(eff1) + geom_line(aes(x = value, y = fit)) +
    xlab('') + ylab('Financial fragility index') +
    geom_ribbon(aes(x = value, ymin = lower, ymax = upper), alpha = 0.2) + 
    #theme_bw() +   
    theme(axis.text = element_text(size = 16), 
                         axis.title=element_text(size=14,face="bold")) +
    scale_colour_manual(values = c('black', 'black')) +
    scale_size_manual(values = c(0.4, 1.2)) +
    scale_linetype_manual(values = c('solid', 'longdash')) +
    scale_x_continuous(expand = c(0.02,0)) +
    scale_y_continuous(breaks=number_ticks(5), expand=c(0.06,0))  
    ggsave( paste0( './Output/peffLev_', cases[i], '.pdf' ), width = 20, height = 16, units = "cm")
  
  ggplot(eff2) + geom_line(aes(x = value, y = fit)) +
    geom_ribbon(aes(x = value, ymin = lower, ymax = upper), alpha = 0.2) + 
    xlab('') + ylab('Financial fragility index') +
    #theme_bw() +   
    theme(axis.text = element_text(size = 16), 
                         axis.title=element_text(size=14,face="bold")) +
    scale_colour_manual(values = c('black', 'black')) +
    scale_size_manual(values = c(0.4, 1.2)) +
    scale_linetype_manual(values = c('solid', 'longdash')) +
    scale_x_continuous(expand = c(0.02,0)) +
    scale_y_continuous(breaks=number_ticks(5), expand=c(0.06,0))  
    ggsave( paste0( './Output/peffProf_', cases[i], '.pdf' ), width = 20, height = 16, units = "cm")
  
  ggplot(eff3) + geom_line(aes(x = value, y = fit)) +
    geom_ribbon(aes(x = value, ymin = lower, ymax = upper), alpha = 0.2) + 
    xlab('') + ylab('Financial fragility index') +
    #theme_bw() +   
    theme(axis.text = element_text(size = 16), 
                         axis.title=element_text(size=14,face="bold")) +
    scale_colour_manual(values = c('black', 'black')) +
    scale_size_manual(values = c(0.4, 1.2)) +
    scale_linetype_manual(values = c('solid', 'longdash')) +
    scale_x_continuous(expand = c(0.02,0)) +
    scale_y_continuous(breaks=number_ticks(5), expand=c(0.06,0))  
    ggsave( paste0( './Output/peffInv_', cases[i], '.pdf' ), width = 20, height = 16, units = "cm")
  
  ggplot(eff4) + geom_line(aes(x = value, y = fit)) +
    geom_ribbon(aes(x = value, ymin = lower, ymax = upper), alpha = 0.2) + 
    xlab('') + ylab('Financial fragility index') +
    #theme_bw() +   
    theme(axis.text = element_text(size = 16), 
                         axis.title=element_text(size=14,face="bold")) +
    scale_colour_manual(values = c('black', 'black')) +
    scale_size_manual(values = c(0.4, 1.2)) +
    scale_linetype_manual(values = c('solid', 'longdash')) +
    scale_x_continuous(expand = c(0.02,0)) +
    scale_y_continuous(breaks=number_ticks(5), expand=c(0.06,0))  
    ggsave( paste0( './Output/peffDist_', cases[i], '.pdf' ), width = 20, height = 16, units = "cm")
  
    # generate data for joint plots
    if( !exists( 'joint_eff' ) ){
      joint_eff <- rbind( eff1, eff2 )
      joint_eff <- rbind( joint_eff, eff3 )
      joint_eff <- rbind( joint_eff, eff4 )
    }
    else{
      joint_eff <- rbind( joint_eff, eff1 )
      joint_eff <- rbind( joint_eff, eff2 )
      joint_eff <- rbind( joint_eff, eff3 )
      joint_eff <- rbind( joint_eff, eff4 )
    }
}                    

# print latex table with coefficients estimates
table_coeffs[ c(1, 3, 5, 7 ), 2:4 ] <- round( table_coeffs[ c(1, 3, 5, 7 ), 2:4 ], 3 )
table_coeffs[ c(2, 4, 6, 8 ), 2:4 ] <- round( table_coeffs[ c(2, 4, 6, 8 ), 2:4 ], 4 )

print(xtable::xtable( table_coeffs, type = "latex"), 
      include.rownames=FALSE, file = paste0( "./Output/coeffs_regression", '.tex' ))

ggplot( joint_eff ) + geom_line(aes( x = value, y = fit, group = exp_n, color = exp_n, linetype = exp_n ) ) +
  geom_ribbon(aes(x = value, ymin = lower, ymax = upper, group = exp_n, fill = exp_n ), alpha = 0.2) + 
  xlab('') + ylab('Financial fragility index') +
  facet_grid( . ~var, scales = 'free_x') +
  #theme_bw() +   
  theme(legend.title=element_blank(), axis.text = element_text(size = plotAxis), 
        legend.text=element_text(size= plotLegend), strip.text.x = element_text(size = plotLegend),
        legend.key.size = unit(1, "cm"), legend.key = element_blank(), legend.position='bottom',
        legend.justification="center",
        legend.box.margin=margin(-10,-10,-10,-10),
        axis.title=element_text(size=plotLegend,face="bold"),) +
  scale_colour_manual(values = c('black', 'red', 'blue' )) +
  scale_fill_manual(values = c('black', 'red', 'blue' )) +
  scale_linetype_manual(values = c('solid', 'longdash', 'dashed')) +
  scale_x_continuous(expand = c(0.03,0)) +
  scale_y_continuous(breaks=number_ticks(5), expand=c(0.06,0)) 
  ggsave( paste0( './Output/det_frag', '.pdf' ), width = 35, height = 15, units = "cm")


rm( eff1, eff2, eff3, eff4, fragReg, frag, vcovNWFrag, joint_eff )
gc(  )

# ===================== Minskian firms and cross correlation with the leverage =====================
minskian <- Adata %>% select( exp_n, t, Minskian ) %>%
  filter( t <= tMax, t >= tMin ) %>%
  mutate( t = t - min( t ) )

minskian_sd <- Sdata %>% select( exp_n, t, Minskian ) %>%
  filter( t <= tMax, t >= tMin ) %>%
  mutate( t = t - min( t ) )

minskian_sd$M_max <- minskian$Minskian + qnorm( 1 - ( 1 - CI ) / 2 ) * minskian_sd$Minskian / sqrt( nSize )
minskian_sd$M_min <- minskian$Minskian - qnorm( 1 - ( 1 - CI ) / 2 ) * minskian_sd$Minskian / sqrt( nSize )

bottom <- minskian_sd %>% select( -Minskian, -M_max ) %>%
  pivot_longer( cols = c('M_min'), values_to = 'Bottom') 
top <- minskian_sd %>% select( -Minskian, -M_min ) %>%
  pivot_longer( cols = c('M_max'), values_to = 'Top') 

colnames(minskian)<- c( 'exp_n', 'Time', 'Minskian' )

minskian$Bottom <- bottom$Bottom
minskian$Top    <- top$Top

for( i in 1:n ){
  subsetData <- minskian %>% ungroup() %>%
    filter( exp_n == cases[i] ) %>%
    select( -exp_n )
  
  ggplot( subsetData, aes(x = Time, y = Minskian ) ) + 
    geom_line(  ) + 
    geom_ribbon( aes( ymin = Bottom, ymax = Top ), alpha = 0.2, colour = NA ) +
    xlab('Time') + ylab( '% of Minskian firms (weighted by size)' ) +
    #theme_bw() +   
    theme(legend.title=element_blank(), axis.text = element_text(size = plotAxis), 
                         legend.text=element_text(size= plotLegend), strip.text.y = element_text(size = plotAxis, angle = 90),
                         legend.key.size = unit(1, "cm"), legend.key = element_blank(), legend.position='bottom',
                         legend.justification="center",
                         legend.box.margin=margin(-10,-10,-10,-10),
                         axis.title=element_text(size=plotLegend,face="bold")) +
    scale_color_manual(values=c("red", "gray20", "gray50")) +
    scale_x_continuous(expand = c(0.02,0)) +
    scale_y_continuous(expand=c(0.04,0))  
    ggsave( paste0( './Output/minskian_', cases[i], '.pdf' ) , width = 20, height = 16, units = "cm")                                  
}                    

rm( minskian, minskian_sd, bottom, top, subsetData )

gc( )

# Cross correlation with leverage
ncor     <- 5
minskian <- mcData %>% select( exp_n, mc_n, t, Minskian, Lev ) %>%
  filter( t <= tMax, t >= tMin ) %>%
  mutate( t = t - min( t ) ) %>%
  mutate( mc_n = as.character( mc_n ) ) %>%
  group_by( exp_n, mc_n ) %>%
  summarise( L = get_ccf( var1 = Lev, var2 = Minskian, ncor = ncor ) ) 

minskian <- cbind( minskian[, 1:2], minskian$L ) %>%
  pivot_longer( cols = c( starts_with( "L" ) ) ) %>%
  mutate( name =  as.numeric( sub( 'L', '', name ) ) )

for( i in 1:n ){
  ggplot(minskian, aes( x = name, y = value, color = 'red' ) ) + 
    stat_summary(geom = 'smooth', se = T,
                 fun.data = "mean_cl_boot", fun.args=list( conf.int= CI, B = 20000 ), alpha = 0.2) +
    xlab('Lag') + ylab('Correlation') +
    #theme_bw() +   
    theme(axis.text = element_text(size = 16), 
                         strip.text.y = element_text(size = 16, angle = 90),
                         legend.position='none',
                         axis.title=element_text(size=14,face="bold")) +
    scale_x_continuous(expand = c(0.02,0), breaks = number_ticks( length( unique( minskian$name ) ) ) ) +
    scale_y_continuous(breaks=number_ticks(5), expand=c(0.04,0))  
    ggsave( paste0( './Output/ccfLevMinskian_', cases[i], '.pdf' ), width = 20, height = 16, units = "cm")
}

rm( minskian, ncor )
# ===================== Single case series =====================
single <- mcData %>% 
  select( exp_n, mc_n, t, FFI, Lev, ProfitRate, I, K, PDIndex ) %>%
  filter( exp_n == expVal[bCase] ) %>%
  group_by( mc_n ) %>%
  mutate( ItoK = I / lag( K ) ) %>%
  select( -I, -K )

cbycase <- single %>% filter( t <= tMax, t >= tMin ) %>%
  summarise( sd = sd( Lev ), max = max( Lev ), min = min( Lev ), med = median( Lev ) )

mc_select <- cbycase$mc_n[which.max( cbycase$sd )]

case_selected <- single %>% filter( mc_n == mc_select ) 

# Leverage
g2 <- case_selected %>% select( t, Lev ) %>%
  mutate( Trend = bkfilter( Lev, pl = 2, pu = highP ,type = c("variable"), drift = F )$trend ) %>% 
  filter( t <= tMax, t >= tMin ) %>%
  mutate( t = t - min( t ) ) %>%
  pivot_longer( cols = c( 'Lev', 'Trend' ) )

ggplot(g2, aes(x = t, y = value, group = name, colour = name, linetype = name, size = name) ) + 
  geom_line(show.legend=F) +
  xlab('Time') + ylab('') +
  #theme_bw() +   
  theme(axis.text = element_text(size = 16), 
                       axis.title = element_text(size=14,face="bold")) +
  scale_colour_manual(values = c('black', 'black')) +
  scale_size_manual(values = c(0.4, 1.2)) +
  scale_linetype_manual(values = c('solid', 'longdash')) +
  scale_x_continuous(expand = c(0.02,0)) +
  scale_y_continuous(breaks=number_ticks(5), expand=c(0.04,0))  
  ggsave('./Output/singleLev.pdf', width = 20, height = 16, units = "cm")

# Fragility index
g3 <- case_selected %>% select( t, FFI ) %>%
  mutate( Trend = bkfilter( FFI, pl = 2, pu = highP ,type = c("variable"), drift = F )$trend ) %>% 
  filter( t <= tMax, t >= tMin ) %>%
  mutate( t = t - min( t ) ) %>%
  pivot_longer( cols = c( 'FFI', 'Trend' ) )

ggplot(g3, aes(x = t, y = value, group = name, colour = name, linetype = name, size = name) ) + 
  geom_line(show.legend=F) +
  xlab('Time') + ylab('') +
  #theme_bw() +   
  theme(axis.text = element_text(size = 16), 
                       axis.title=element_text(size=14,face="bold")) +
  scale_colour_manual(values = c('black', 'black')) +
  scale_size_manual(values = c(0.4, 1.2)) +
  scale_linetype_manual(values = c('solid', 'longdash')) +
  scale_x_continuous(expand = c(0.02,0)) +
  scale_y_continuous(breaks=number_ticks(5), expand=c(0.04,0))  
  ggsave('./Output/singleFragIndex.pdf', width = 20, height = 16, units = "cm")

# Profit rate
g4 <- case_selected %>% select( t, ProfitRate ) %>%
  mutate( Trend = bkfilter( ProfitRate, pl = 2, pu = highP ,type = c("variable"), drift = F )$trend ) %>% 
  filter( t <= tMax, t >= tMin ) %>%
  mutate( t = t - min( t ) ) %>%
  pivot_longer( cols = c( 'ProfitRate', 'Trend' ) )

ggplot(g4, aes(x = t, y = value, group = name, colour = name, linetype = name, size = name) ) + 
  geom_line(show.legend=F) +
  xlab('Time') + ylab('') +
  #theme_bw() +  
  theme(axis.text = element_text(size = 16), 
                       axis.title=element_text(size=14,face="bold")) +
  scale_colour_manual(values = c('black', 'black')) +
  scale_size_manual(values = c(0.4, 1.2)) +
  scale_linetype_manual(values = c('solid', 'longdash')) +
  scale_x_continuous(expand = c(0.02,0)) +
  scale_y_continuous(breaks=number_ticks(5), expand=c(0.04,0))  
  ggsave('./Output/singleProfRate.pdf', width = 20, height = 16, units = "cm")

# Profit-debt distribution
g5 <- case_selected %>% select( t, PDIndex ) %>%
  mutate( Trend = bkfilter( PDIndex, pl = 2, pu = highP ,type = c("variable"), drift = F )$trend ) %>% 
  filter( t <= tMax, t >= tMin ) %>%
  mutate( t = t - min( t ) ) %>%
  pivot_longer( cols = c( 'PDIndex', 'Trend' ) )

ggplot(g5, aes( x = t, y = value, group = name, colour = name, linetype = name, size = name) ) + 
  geom_line(show.legend=F) +
  xlab('Time') + ylab('') +
  #theme_bw() +   
  theme(axis.text = element_text(size = 16), 
                       axis.title=element_text(size=14,face="bold")) +
  scale_colour_manual(values = c('black', 'black')) +
  scale_size_manual(values = c(0.4, 1.2)) +
  scale_linetype_manual(values = c('solid', 'dashed')) +
  scale_x_continuous(expand = c(0.02,0)) +
  scale_y_continuous(breaks=number_ticks(5), expand=c(0.04,0))  
  ggsave('./Output/singleDist.pdf', width = 20, height = 16, units = "cm")

# Investment ratio 
g6 <- case_selected %>% select( t, ItoK ) %>% filter( !is.na( ItoK ) ) %>%
  mutate( Trend = bkfilter( ItoK, pl = 2, pu = highP ,type = c("variable"), drift = F )$trend ) %>% 
  filter( t <= tMax, t >= tMin ) %>%
  mutate( t = t - min( t ) ) %>%
  pivot_longer( cols = c( 'ItoK', 'Trend' ) )

ggplot(g6, aes( x = t, y = value, group = name, colour = name, linetype = name, size = name ) ) + 
  geom_line(show.legend=F) +
  xlab('Time') + ylab('') +
  #theme_bw() +   
  theme(axis.text = element_text(size = 16), 
                       axis.title=element_text(size=14,face="bold")) +
  scale_colour_manual(values = c('black', 'black')) +
  scale_size_manual(values = c(0.4, 1.2)) +
  scale_linetype_manual(values = c('solid', 'dashed')) +
  scale_x_continuous(expand = c(0.02,0)) +
  scale_y_continuous(breaks=number_ticks(5), expand=c(0.04,0))  
  ggsave('./Output/singleInvRate.pdf', width = 20, height = 16, units = "cm")


# ===================== Unit root tests =====================
ur.tests <- mcData %>% 
  select( exp_n, mc_n, t, statErgo.vars ) %>%
  group_by( exp_n, mc_n ) %>%
  summarise( across( statErgo.vars, ~try(suppressWarnings(tseries::adf.test( .x ) $p.value), silent = TRUE), .names = "ADF_{.col}" ),
             across( statErgo.vars, ~try(suppressWarnings(stats::PP.test( .x ) $p.value), silent = TRUE), .names = "PP_{.col}" ),
             across( statErgo.vars, ~try(suppressWarnings(tseries::kpss.test( .x )$p.value), silent = TRUE), .names = "KPSS_{.col}" ) ) %>%
  group_by( exp_n ) %>% 
  summarise( across( contains( 'ADF' ), ~sum(.x < 1 - CI ) / n( ) ),
             across( contains( 'PP' ), ~sum(.x < 1 - CI ) / n( ) ),
             across( contains( 'KPSS' ), ~sum(.x < 1 - CI ) / n( ) ) ) %>%
  pivot_longer( cols = -exp_n ) %>%
  separate( name, c( "Test", "Variable" ), sep = '_' ) %>% 
  pivot_wider( names_from = 'Test' )

# ===================== Distribution of leverage =====================
mcData %>% group_by( exp_n ) %>% 
  summarise( bottom = quantile( Lev, 0.025 ), upper = quantile( Lev, 0.975 ) )

