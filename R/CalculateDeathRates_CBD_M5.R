#' Calculate the modelled crude death rategiven the parameters provided assuming a CBD M5 model
#' 
#' CBD M5 is an extension of Lee-Carter with time varying intercept which gives
#'    $$ logit q(x,t) = \kappa^{1}_t + \kappa^{2}_t (x - \bar{x}) $$
#'    
#' 
#' The params object is a list with elements kappa1 and kappa2, which must have the same length as the range of MortalityData$Year
#' 
#' @param t       a vector of times
#' @param x       a vector of ages
#' @param params  a list containing two vectors of parameters called kappa1 and kappa2, indexed by t
#' 
#' @return The log-likelihood
#' 
CalculateDeathRates_CBD_M5 <- function( t, x, params )  {

  library( boot )
  
  x_bar <- mean( x )
  
  q_t_x <- boot::inv.logit( params$kappa1[t] + params$kappa2[t] %o% ( x - x_bar ) )
  
  stopifnot( all( (q_t_x <= 1) & (q_t_x >= 0) ) )
  
  m_t_x <- - log( 1 - q_t_x )
    
  return( m_t_x )

}
