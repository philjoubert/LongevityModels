#' Calculate the Poisson log likelihood of the mortality model
#' 
#' Takes a matrix of deaths, one of exposures and one of modelled mortality rates
#' 
#' @param Deaths a matrix of observed numbers of deaths
#' @param Exposures a matrix of exposures
#' @param Model a matrix of crude death rates
#' 
CalculateLogLikelihood <- function( Deaths, Exposures, Model ){
  
  ExpectedDeaths <- Exposures * Model
  
  LogLikelihood <- Deaths * log( ExpectedDeaths ) - ExpectedDeaths - lfactorial ( Deaths )

  return( sum ( LogLikelihood ) )
  
}