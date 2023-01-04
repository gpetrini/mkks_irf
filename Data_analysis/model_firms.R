#******************************************************************
#
# ------------------ K+S sector 1 analysis ----------------------
#
#******************************************************************

#******************************************************************
#
# ------------ Read Monte Carlo experiment files ----------------
#
#******************************************************************
if( exists( 'mcData' ) ) rm (mcData) 
number_ticks <- function(n) {function(limits) pretty(limits, n)}

folder   <- "./Base_scen"             # data files folder
baseName <- "S"                       # data files base name (same as .lsd file)
nExp     <- 1                         # number of experiments
iniDrop  <- 100                       # initial time steps to drop from analysis (0=none)
nKeep    <- -1                        # number of time steps to keep (-1=all)

expVal <- c( "S1", "S2", "S3" )       # case parameter values

# Firm-level variables to use
firmVar  <- c( "__Lev", "__Lf", "__Assets", "__int", "__KN", "__ProfitRate" )
sector   <- "( Capital-goods sector )"


# ==== Process LSD result files ====

# Package with LSD interface functions
library( LSDinterface, verbose = FALSE, quietly = TRUE )
library( arrayhelpers, verbose = FALSE, quietly = TRUE )
library( dplyr, verbose = FALSE, quietly = TRUE )
library( tidyr, verbose = FALSE, quietly = TRUE )
library( ggplot2, verbose = FALSE, quietly = TRUE )


# remove warnings for saved data
# !diagnostics suppress = mc, pool, nSize, nTsteps

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
  
  # Read data from text files and format it as 4D array with labels
  mc <- read.4d.lsd( myFiles, col.names = firmVar, skip = iniDrop,
                     nrows= nKeep, nnodes = 1 )
  
  # Get dimensions information
  nTsteps <- dim( mc )[ 1 ]              # number of time steps
  nFirms <- dim( mc )[ 3 ]               # number of firms (instances)
  nSize  <- dim( mc )[ 4 ]               # Monte Carlo sample size
  
  tmpFile <- paste0( folder, "/", baseName, '_', exper, "_firm1.Rdata" )
  save( mc, nTsteps, nSize, nFirms, file = tmpFile )
  
  return( tmpFile )
}

# load each experiment serially
tmpFiles <- lapply( 1 : nExp, readExp )

# ---- Organize data read from files ----

# fill the lists to hold data
nTsteps.1 <- nSize.1 <- 0

for( k in 1 : nExp ) {                      # realocate data in separate lists
  
  load( tmpFiles[[ k ]] )                   # pick data from disk
  file.remove( tmpFiles[[ k ]] )            # and delete temporary file

  dimnames(mc)[[4]] <- rep( paste0( 'mc_', 1:dim(mc)[4]) ) ## rename monte carlo dimension
  
  tmpData <- array2df( mc, na.rm = TRUE )
  colnames(tmpData) <- c( 'Value', 't', 'Variable', 'Firm', 'mc_n' )
  tmpData <- tmpData %>% pivot_wider( values_from = 'Value', names_from = 'Variable' )
  tmpData$exp_n <- rep( expVal[ k ], nrow( tmpData ) ) 
  tmpData <- tmpData %>% relocate( exp_n, mc_n, t, Firm )
  colnames(tmpData) <- stringr::str_remove( string = colnames(tmpData), "X__" )
  tmpData$t <- as.numeric( tmpData$t ) + iniDrop
  
   if( !exists( 'mcData' ) )
   {
     mcData <- tmpData
   }
   else
   {
     mcData  <- rbind( mcData, tmpData )
   }
  rm( mc, tmpData )
}

# free memory
invisible( gc( verbose = FALSE ) )

#******************************************************************
#
# --------------------- Remove first period of entrant firms ------
#
#******************************************************************
minT   <- min( mcData$t )
mcData <- mcData %>% group_by( exp_n, mc_n, Firm ) %>%
  mutate( flagRemove1 = ifelse( min( t ) == minT, 0, 1 ) ) %>%
  mutate( flagRemove2 = ifelse( flagRemove1 & t == min( t ), 1, 0  ) ) %>%
  filter( flagRemove2 != 1 ) %>%
  select( -flagRemove1, -flagRemove1, -flagRemove2 ) %>%
  ungroup()

#******************************************************************
#
# --------------------- Plot statistics -------------------------
#
#******************************************************************

# ====== User parameters ======

CI     <- 0.95   # desired confidence interval
nBins  <- 15     # number of bins to use in histograms
outLim <- 0.001  # outlier percentile (0=don't remove outliers)
warmUp <- 300    # number of "warm-up" runs
nTstat <- -1     # last period to consider for statistics (-1=all)
bCase  <- 1      # experiment to be used as base case


repName <- ""    # report files base name (if "" same baseName)
sDigits <- 4     # significant digits in tables
plotRows <- 1    # number of plots per row in a page
plotCols <- 1  	 # number of plots per column in a page
plotW <- 10      # plot window width
plotH <- 7       # plot window height

# Colors assigned to each experiment's lines in graphics
colors <- c( "black", "blue", "red", "orange", "green", "brown" )

# Line types assigned to each experiment
lTypes <- c( "solid", "solid", "solid", "solid", "solid", "solid" )

# Point types assigned to each experiment
pTypes <- c( 4, 4, 4, 4, 4, 4 )


###########################################################
# Classify firms into categories of leverage 
# (= 0, 0 < Lev < 10%, 10% <= Lev < 20, ... , Lev > 90%)
# and plot transitional propabilities
###########################################################
catLev <- mcData %>% select( exp_n, mc_n, t, Firm, Lev ) %>%
  mutate( levClass = ifelse(Lev == 0, 'D = 0', 
                            ifelse(Lev > 0 & Lev <= 0.1, '0 < D ≤ 0.1', 
                                   ifelse(Lev > 0.1 & Lev <= 0.2, '0.1 < D ≤ 0.2', 
                                          ifelse(Lev > 0.2 & Lev <= 0.3, '0.2 < D ≤ 0.3', 
                                                 ifelse(Lev > 0.3 & Lev <= 0.4, '0.3 < D ≤ 0.4',
                                                        ifelse(Lev > 0.4 & Lev <= 0.5, '0.4 < D ≤ 0.5', 
                                                               ifelse(Lev > 0.5 & Lev <= 0.6, '0.5 < D ≤ 0.6',
                                                                      ifelse(Lev > 0.6 & Lev <= 0.7, '0.6 < D ≤ 0.7',
                                                                             ifelse(Lev > 0.7 & Lev <= 0.8, '0.7 < D ≤ 0.8',
                                                                                    ifelse(Lev > 0.8 & Lev <= 0.9, '0.8 < D ≤ 0.9',
                                                                                           ifelse(Lev > 0.9, 'D > 0.9', NA))))))))))))
catLev <- catLev %>% arrange( exp_n, mc_n, Firm, t ) %>%
  group_by( exp_n, mc_n, Firm ) %>%
  mutate( llevClass = lead( levClass ) ) %>%
  filter( !is.na( llevClass ) ) %>% ungroup()

freqTot<- catLev %>% group_by( exp_n, levClass ) %>%
  summarise( tot = n( ) )

freqClas<- catLev %>% group_by( exp_n, levClass, llevClass ) %>%
  summarise( N = n( ) )
freq    <- merge(freqClas, freqTot, all.x = T) %>%
  mutate( perc = N / tot ) %>%
  mutate( levClass = as.factor( levClass ), llevClass = as.factor( llevClass ) ) %>%
  mutate( levClass = gdata::reorder.factor(levClass, new.order = c(10, 1:9, 11) ),
          llevClass := gdata::reorder.factor(llevClass, new.order = c(10, 1:9, 11)) )

for( i in 1 : length( expVal ) ){
  ggplot(freq %>% filter( exp_n == expVal[i] ), aes(x = levClass, y = llevClass, fill = perc)) + geom_tile() + 
    scale_fill_gradient2(low = "grey80", high = "gray0", mid = "gray40", 
                         midpoint = 0.5, limit = c(0,1), space = "Lab", na.value = 'white') + 
    xlab('Group of leverage in (t)') + ylab('Group of leverage in (t + 1)') +
    theme_bw() +
    geom_text(aes(levClass, llevClass, label = round(perc, digits = 3)), color = "white", size = 5.3) +
    theme(
      panel.grid.major = element_blank(), panel.background = element_blank(), axis.ticks = element_blank(),
      legend.position = 'bottom', legend.direction = "horizontal", legend.title=element_blank(), 
      legend.text=element_text(size= 14),  axis.title=element_text(size = 16,face="bold"), 
      axis.text.x = element_text(angle = 45, vjust = 1, size = 14, hjust = 1),
      axis.text.y = element_text(angle = 45, size = 14)) +
    guides(fill = guide_colorbar(barwidth = 15, barheight = 1)) +
    ggsave(  paste0( './Output/transition_', expVal[i], '.pdf' ), width = 20, height = 16, units = "cm", device=cairo_pdf)  
}

rm( freq, freqTot, freqClas, catLev )

gc()

####################################################################
# Which quartile of leverage commands the aggregate leverage?
####################################################################
comLev  <- mcData %>% 
  select( exp_n, mc_n, t, Firm, Lf, Lev, Assets, int ) %>%
  group_by( exp_n, mc_n, t ) %>%
  mutate( qLev = ntile( Lev, 4 ) ) %>%
  group_by( exp_n, mc_n, t, qLev ) %>%
  summarise( levQ = sum( Lf ) / sum( Assets ), AssetsQ = sum( Assets ), avgInt = sum( int * Lf ) / sum( Lf ) ) %>%
  group_by( exp_n, mc_n, t ) %>%
  mutate( AS = AssetsQ / sum( AssetsQ ) ) %>%
  mutate( contribLev = AS * levQ ) %>%
  mutate( qLev = as.factor( qLev ) ) 

# comLevBP<- mcData %>% 
# select( exp_n, mc_n, t, Lev ) %>%
# group_by( exp_n, mc_n, t ) %>%
# mutate( qLev = ntile( Lev, 4 ) )  %>%
# group_by( exp_n, mc_n, t, qLev ) %>%
# summarise( coefVar = sd( Lev ) / mean( Lev ) ) %>%
# mutate( qLev = as.factor( qLev ) ) 

  

levels(comLev$qLev) <- c('1st quartile', '2nd quartile', '3rd quartile', '4th quartile')

# load macro data 
load( paste0( folder, "/mcData_aggr.Rdata" ) )

# merge with comlev
comLev <- merge( comLev, exportData %>% select( -Lev ) )

# for( i in 1 : length( expVal ) ){
#   teste <- lm( contribLev ~ 1 +  qLev * PDIndex + avgInt, data = comLev %>% filter( exp_n == expVal[i] ) )
#   print( summary( teste ))
# }

for( i in 1 : length( expVal ) ){
  ggplot( comLev %>% filter( exp_n == expVal[i] ), aes(x = contribLev, y = PDIndex) ) +
    facet_grid(qLev ~ .) + 
    ylab('Profit-debt distribution index') + xlab('Contribution of quartile to total leverage') +
    stat_density2d( geom = "polygon", colour = 'grey20', fill = 'grey70', alpha = 0.7) +
    scale_fill_viridis_c()  +
    theme(axis.text = element_text(size = 20), 
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20),
          strip.text.y = element_text(size = 20),
          legend.key.size = unit(1, "cm"), legend.key = element_blank(), legend.position='bottom',
          legend.justification="center",
          legend.box.margin=margin(-10,-10,-10,-10),
          axis.title = element_text(size = 20,face="bold"),
          panel.grid.minor = element_blank()) +
    scale_x_continuous(expand = c(0, 0)) +
    ggsave(  paste0( './Output/levContribQ_', expVal[i], '.pdf' ), width = 40, height = 25, units = "cm", device = cairo_pdf)  
}

# for the box plot
comLevBP<- comLev %>% group_by( exp_n, mc_n, qLev ) %>%
  mutate( meanContriQ = mean( contribLev ) ) %>%
  mutate( contribPerc = contribLev / meanContriQ - 1 ) %>%
  summarise( coefVar = sd( levQ ) / mean( levQ ) ) %>%
  filter( ntile( coefVar, 100 ) < 95 )
levels(comLevBP$qLev) <- c('1st', '2nd', '3rd', '4th')

# box plot
ggplot( comLevBP %>% filter( exp_n %in% c( 'S1', 'S2', 'S3' ) ), aes( x = qLev, y = coefVar, fill = qLev ) ) + 
  geom_boxplot( outlier.shape = NA, alpha = 0.6 ) +
  facet_grid( . ~ exp_n ) +
  ylab('Coefficient of variation (leverage)'  ) + xlab( 'Quartile of leverage' ) +
  #theme_bw() +
  theme(axis.text = element_text(size = 16), 
        strip.text.y = element_text(size = 20),
        legend.key.size = unit(1, "cm"), legend.key = element_blank(),
        legend.position='none',
        legend.justification="center",
        legend.box.margin=margin(-10,-10,-10,-10),
        axis.title = element_text(size = 16 ),
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 16, colour = "black") ) +
  scale_fill_brewer(palette="Set1") +
  ggsave(  './Output/coef_var_levQ.pdf' , width = 35, height = 15, units = "cm", device = cairo_pdf)  



gc()

# ===================== Autocorrelation of aggregate leverage =====================
ncor <- 10

get_ccf <- function( var, ncor ) {
  bind_rows( setNames( 
    c( ccf( var, var, 
            na.action = na.pass, plot = F, lag.max = ncor)$acf[,,1] ), 
    paste0( 'L', -ncor:ncor ) ) )
}

firmLev         <- mcData %>%
  select( exp_n, mc_n, t, Firm, Lev ) %>%
  mutate( mc_n = as.character( mc_n ) ) %>%
  group_by( exp_n, mc_n, Firm ) %>%
  mutate( nObs = n( ) ) %>%
  filter( nObs > 3 * ncor ) %>%
  summarise( L = get_ccf( Lev, ncor = ncor ) )

firmLev <- cbind( firmLev[, 1:3], firmLev$L ) %>% 
  group_by( exp_n, mc_n ) %>%
  summarise( across( starts_with( "L" ), mean ) ) %>%
  pivot_longer( cols = c( starts_with( "L" ) ) ) %>%
  mutate( group = 'Firm-level' )

agLev <- exportData %>%
  select( exp_n, mc_n, t, Lev ) %>%
  mutate( mc_n = as.character( mc_n ) ) %>%
  group_by( exp_n, mc_n ) %>%
  summarise( L = get_ccf( Lev, ncor = ncor ) ) 

agLev <- cbind( agLev[, 1:2], agLev$L ) %>%
  pivot_longer( cols = c( starts_with( "L" ) ) ) %>%
  mutate( group = 'Aggregate' )

pdata               <- rbind(firmLev, agLev)
pdata$name          <- as.numeric( sub( 'L', '', pdata$name ) )

for( i in 1 : length( expVal ) ){
  ggplot(pdata %>% filter( exp_n == expVal[i] ), 
         aes(x = name, y = value, group = group, color = group, linetype = group, size = group)) + 
    stat_summary(geom = 'smooth', se = TRUE, 
                 fun.data = "mean_cl_boot", fun.args=list(conf.int=0.95, B = 20000), alpha = 0.2) +
    xlab('Lag') + ylab('Correlation') +
    #theme_bw() +   
    theme(legend.title=element_blank(), axis.text = element_text(size = 16), 
                         legend.text=element_text(size= 14), strip.text.y = element_text(size = 16, angle = 90),
                         legend.key.size = unit(1, "cm"), legend.key = element_blank(), legend.position='bottom',
                         legend.justification="center",
                         legend.box.margin=margin(-10,-10,-10,-10),
                         axis.title=element_text(size=14,face="bold")) +
    scale_color_manual(values=c("black", "black")) +
    scale_size_manual(values = c(0.4, 1)) +
    scale_x_continuous(expand = c(0.02,0)) +
    scale_y_continuous(breaks=number_ticks(5), expand=c(0.04,0))  +
    ggsave( paste0( './Output/levAutocorrelation_', expVal[i], '.pdf' ), width = 20, height = 16, units = "cm")
}

rm( agLev, firmLev, pdata )

gc( )

# ===================== Illustration of concentration curves =====================
agData           <- exportData %>% select( exp_n, mc_n, t, PDIndex ) %>%
  filter( exp_n == expVal[ bCase ], mc_n == 'mc_1', t > iniDrop )

mald <- agData  %>%
  group_by( mc_n ) %>%
  summarise( maxMald = max( PDIndex ), minMald = min( PDIndex ) ) %>%
  mutate( var = maxMald - minMald ) %>%
  filter( var == max( var ) )

caseSelected <- agData[c( which( agData$PDIndex == mald$maxMald ), which( agData$PDIndex == mald$minMald ) ), ]

concPL           <- mcData %>% filter( exp_n == expVal[ bCase ], mc_n == unique( caseSelected$mc_n ) ) %>%
  select( t, mc_n, Firm, Lev, KN, ProfitRate, Lf ) %>%
  arrange( Firm, t ) %>%
  group_by( Firm ) %>%
  mutate( nomProfit = ProfitRate * lag( KN ),
          lLf = lag( Lf ), 
          lLev = lag( Lev ) ) %>%
  filter( t %in% c( caseSelected$t, caseSelected$t - 1) )

# Load the function to calculate the profit-debt distribution index
source("./Support_Functions/distIndex.R", local = T )

tmax <- with( caseSelected, t[which.max( -PDIndex )] )
tmin <- with( caseSelected, t[which.min( -PDIndex )] )

aux1             <- with( concPL %>% filter( t == tmax ), distIndex(x = nomProfit, y = lLf, w = lLev, returnData = T)$data )
aux2             <- with( concPL %>% filter( t == tmin ), distIndex(x = nomProfit, y = lLf, w = lLev, returnData = T)$data )
concCurve45      <- data.frame(cumProf = seq(0, 1, length.out = nrow(aux1)))        
pconc            <- data.frame(rbind(cbind(typ = 'Highest concentration', cpy = aux1$cpy, cpx = aux1$cpx),
                                     cbind(typ = 'Lowest concentration', cpy = aux2$cpy, cpx = aux2$cpx),
                                     cbind(typ = 'Equality line', cpy = concCurve45$cumProf, cpx = concCurve45$cumProf)))
pconc$cpx <- as.numeric( as.character(pconc$cpx) )
pconc$cpy <- as.numeric( as.character(pconc$cpy) )

ggplot(data = pconc, aes(x = cpy, y = cpx, group = typ, color = typ, linetype = typ, size = typ)) + geom_line() +
  xlab('Cumulative share of debt from lowest to highest leverage') + ylab('Cumulative share of profit') + 
  #theme_bw() +  
  theme(legend.title=element_blank(), axis.text = element_text(size = 16), 
                      legend.text=element_text(size= 14), strip.text.y = element_text(size = 16, angle = 90),
                      legend.key.size = unit(1, "cm"), legend.key = element_blank(), legend.position='bottom',
                      legend.justification="center",
                      legend.box.margin=margin(-10,-10,-10,-10),
                      axis.title=element_text(size=14,face="bold")) +
  scale_color_manual(values = c("black", "gray10", "gray50")) +
  scale_linetype_manual(values = c("solid", "dashed", "dotdash")) +
  scale_size_manual(values = c(0.5, 0.5, 1.5)) +
  scale_x_continuous(expand = c(0, 0.02)) +
  scale_y_continuous(expand = c(0, 0.02)) +
  ggsave('./Output/profitVSdebtDist.pdf', width = 20, height = 16, units = "cm")

# ===================== Percentage of firms with zero leverage =====================
mcData %>% group_by( exp_n ) %>% filter( t > iniDrop ) %>%
summarise( percZeroLev = sum( Lev == 0 ) / n() )


