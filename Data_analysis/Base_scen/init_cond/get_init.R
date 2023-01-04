#!/usr/bin/env Rscript
args = commandArgs( trailingOnly =  TRUE )

if ( grepl( '\\/', args[ 1 ] )  )
{
  # get latest position of bar character
  pos <- gregexpr( '\\/', args[1] )[[1]]
  pos <- max( pos )
  args[ 1 ] <- substr( args[ 1 ], pos + 1, nchar( args[ 1 ] ) )
}

library(sfcr)

# Model initial condition
folder      <- args[2]
varList     <- c( "gk", "inf", "u", "K", "nu", "Nf", "gamma", "iota", "h", "Gamma0", "lev", "Assets",
               "r", "theta", "nk", "ibk", "lambda", "tau2", "i", "etab", "gDebtGDP", "beta", "alpha1" )

outputVars <- c( 'gss', 'uT', 'A', 'S', 'INV', 'L', 'P', 'w', 'Df', 'etaf', 'Dh', 'V', 
                 'Bh', 'alpha2', 'spread', 'nwB', 'Bb', 'B', 'tau1' )

varListTxt  <- paste0( args[1], "_", varList, ".txt" )

tmpFiles    <- list.files( folder )
tmpFiles    <- tmpFiles[ tmpFiles %in% varListTxt ]

lf <- list( )
for( i in 1 : length( tmpFiles ) ){
    lf[[i]] <- as.numeric( read.table( paste0( folder, tmpFiles[ i ] ) ) )
}

lf <- as.data.frame( do.call( cbind, lf ) )
colnames( lf ) <- varList[ order( varList ) ]

# Create the system of simultaneous equations using package sfcr
eqs <- sfcr_set(
  gss ~ (1 + gk) * (1 + inf) - 1,
  Y ~ Yp * u,
  Yp ~ K / ( nu * ( 1 + gk ) ),
  uT ~ u - 0.02,
  A ~ (u * K + nu * RD * (1 + gk) )/( nu * Nf ),
  RD ~ gamma * S /(1 + gk),
  S ~ Y - ( INV * gk )/( 1 + gk ),
  INV ~ iota * S,
  C ~ S - I - G,
  I ~ h * Y, 
  G ~ ( Gamma0 * K ) / ( 1 + gk ),
  L ~ lev * Assets,
  P ~ ( Assets - Df ) / ( r * K + INV / ( 1 + theta ) ),
  ucf ~ P / ( 1 + theta ),
  w ~ ucf * A,
  piG ~ P * S - w * Nf  - ibk * L / ( 1 + gss ),
  DepKN ~ ( nk * P * K * r ) / ( 1 + gss ),
  piN ~ piG + ( gss * ucf * INV ) / (1 + gss) - DepKN,
  ra ~ ( gss * sumAm ) / ( lambda - sumAm ),
  sumAm ~ sum( ( 1 / ( 1 + gss ) ) ^ ( 1 : lambda ) ),
  AM ~ ( gss + ra ) / ( lambda * (1 + gss) ) * L * sumAm,
  Df ~ ( P * I + w * Nf - AM - L * gss / ( 1 + gss ) ) * ( 1 + gss ),
  piDF ~ gss * ( L - Df ) / ( 1 + gss ) + piG - tau2 * piN - P * I,
  piB ~ ( ibk * L + i * Bb ) / ( 1 + gss ),
  piDB ~ etab * (1 - tau2 ) * piB,
  etaf ~ piDF / ( ( 1 - tau2 ) * piN  ),
  nwB ~ ( ( 1 - tau2 ) * piB - piDB ) * ( 1 + gss ) / gss,
  B ~ gDebtGDP * P * Y,
  Bh ~ V - Dh,
  Bb ~ B - Bh,
  Dh ~ beta * V,
  spread ~ ibk - i,
  tau1 ~ ( P * G - tau2 * ( piB + piN ) - B * ( gss - i ) / ( 1 + gss ) ) / ( w * Nf + piDF + piDB + i * Bh / ( 1 + gss ) ),
  V ~ ( ( 1 - alpha1 ) * ( 1 - tau1 ) * w * Nf + ( 1 - tau1 ) * ( piDF + piDB + i * Bh / ( 1 + gss ) ) ) * ( 1 + gss ) / ( gss + alpha2 ),
  alpha2 ~ ( P * C - alpha1 * ( 1 - tau1 ) * w * Nf ) * ( 1 + gss ) / V
)

# Input data from LSD initial pre-SS parameters
external <- sfcr_set(
  gk ~ lf$gk,
  inf ~ lf$inf,
  u ~ lf$u,
  K ~ lf$K,
  nu ~ lf$nu,
  Nf ~ lf$Nf,
  gamma ~ lf$gamma,
  iota ~ lf$iota,
  h ~ lf$h,
  Gamma0 ~ lf$Gamma0,
  lev ~ lf$lev,
  Assets ~ lf$Assets,
  r ~ lf$r,
  theta ~ lf$theta,
  nk ~ lf$nk,
  ibk ~ lf$ibk,
  lambda ~ lf$lambda,
  tau2 ~ lf$tau2,
  i ~ lf$i,
  etab ~ lf$etab,
  gDebtGDP ~ lf$gDebtGDP,
  beta ~ lf$beta,
  alpha1 ~ lf$alpha1
)

# Solve model using Broyden's algorithm
sim <- sfcr_baseline(
  equations = eqs, 
  external = external,
  periods = 2,
  method = 'Gauss'
)

# Save output and store parameter history
output <- sim[-1, colnames( sim ) %in% outputVars ]

for ( i in 1 : length( colnames( output ) ) ){
  write.table( output[1, i], file = paste0( folder, args[1], '_', colnames(output)[i], '.txt' ), 
               append = F, quote = F, col.names = F, row.names = F )  
}
print( output )


# if ( 'parameterSet.RData' %in% list.files( folder ) ){
#   load( paste0( folder, 'parameterSet.RData' ) )
#   pars <- data.frame( seed = args[1] )
#   pars <- cbind( pars, sim[2, -1] )
#   parameters <- rbind( parameters, pars )
# } else {
#   parameters <- data.frame( seed = args[1] )
#   parameters <- cbind( parameters, sim[2, -1] )
# }
# 
# save( parameters, file = paste0( folder, 'parameterSet.RData' ) )

# remove input parameters
for( i in 1 : length( tmpFiles ) ){
  file.remove( paste0( folder, tmpFiles[ i ] ) )
}

cat( "\nR code successfully run...")