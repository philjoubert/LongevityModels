#' Calculate the modelled crude death rategiven the parameters provided assuming a CBD M5 model
#' 
#' CBD M5 is an extension of Lee-Carter with time varying intercept which gives
#'    #eqn{ logit q(x,t) = \kappa^{1}_t + \kappa^{2}_t (x - \bar{x}) }
#'    
#' The params object is a dataframe with elements kappa1 and kappa2, which must have the same length as the range of MortalityData$Year
#' 
#' @param dfDataDomain A dataframe containing columns Year and Age defining the domain of the data set
#' @param dfParams  a dataframe containing two columns of parameters called kappa1 and kappa2, indexed by t
#' 
#' @return The log-likelihood
#' 
ModelDeathRatesM5 <- function( dfDataRange, params )  {

  library( boot )

  #TODO: Change this to apply mean over the year sbubsets
  x_bar <- mean( x )
  
  q_t_x <- boot::inv.logit( params$kappa1[t] + params$kappa2[t] %o% ( x - x_bar ) )
  
  stopifnot( all( (q_t_x <= 1) & (q_t_x >= 0) ) )
  
  m_t_x <- - log( 1 - q_t_x )
    
  return( m_t_x )

}
