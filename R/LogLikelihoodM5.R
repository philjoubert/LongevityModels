#' Calculate the log likelihood of the M5 model, given the data and parameter set
#' 
#' 
#' @param dfData A dataframe containing columns Year, Age, dx (Deaths) and lx (Exposures)
#' @param vnParams A vector containing the parameters of the model \eqn{ \kappa^{(1)}_t, \kappa^{(2)}_t}
#' 
#' @return the log likelihood as a number
#' 
LogLikelihoodM5 <- function( dfData, vnParams ) {
  
  t <- pmax(dfData$Year) - pmin(dfData$Year) + 1
  
  x <- pmax(dfData$Age) - pmin(dfData$Age) + 1
  
  ModelRates <- ModelDeathRatesM5( t, x, vnParams)
  
  
}