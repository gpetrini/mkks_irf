get_ccf <- function( var1, var2, ncor ) {
  bind_rows( setNames( 
    c( ccf( var1, var2, 
            na.action = na.pass, plot = F, lag.max = ncor)$acf[,,1] ), 
    paste0( 'L', -ncor:ncor ) ) )
}
