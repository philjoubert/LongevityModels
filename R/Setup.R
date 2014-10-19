#' This script will load the data and create the Model list
#' 

library(Longevity)

#' Load the data
yrs <- 1960:2009
x <- 55:90
t <- 1:length(yrs)

filename <- "Australian_mortality_mltper_1x1.txt"
mortality_table <- LoadMortalityData( filename )
Deaths <- ExtractDeathsFromMortalityTable( mortality_table, Years= yrs, Ages= x )
Exposures <- ExtractExposuresFromMortalityTable( mortality_table, Years= yrs, Ages= x )



#' Create Model list

#' Cairns, Blake, Dowd model M5
#'



#' Number of parameters
k <- 2 * length(yrs)



#' Parameter priors - kappa's will be similar, so assume they are highly correlated in each type

correlations <- 0.99^outer( 1:(k/2), 1:(k/2), function(x,y) abs(x-y) )

ParameterPriorVariance <- matrix( 0, nrow= k, ncol=k )
ParameterPriorVariance[1:(k/2), 1:(k/2)] <- correlations
ParameterPriorVariance[(1+k/2):k, (1+k/2):k] <- correlations

ParameterPriorVariance <- diag( 10, k ) %*% ParameterPriorVariance %*%  diag( 10, k )

ParameterPriorMean <- rep(0, k)

#' Create the model object

CairnsBlakeDowdM5 <- list(

  NumberParameters= k, 
  
  #' The Model object must have an element Likelihood which returns the likelihood at the given parameters
  Likelihood= function( theta ) {
  
    model_death_rates <- CalculateModelDeathRatesCBDM5( t, x, theta )
    
    log_likelihood <- CalculateLogLikelihood( Deaths, Exposures, model_death_rates )
    
    return( exp( log_likelihood ) )
  },
  
  
  #' The Model object must have an element ParameterPrior which returns the prior density of the given parameters
  ParameterPrior= function( theta ) {
    return( dmvnorm( theta, ParameterPriorMean, ParameterPriorVariance ) )
  }
  
)

#' List of currently known models
#' 
Models <-list( CairnsBlakeDowdM5 )

