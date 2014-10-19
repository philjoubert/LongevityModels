#' Load Mortality Data in HMD format from the given filename
#' 
#' @param filename A filename (possibly including paths)
#' 
#' @return A dataframe containing the mortality datas
#' 
LoadMortalityData <- function( filename ){
  
  MortalityData <- read.table(filename, skip=2, header=TRUE)
  MortalityData$Age <- as.integer( gsub("+", "", MortalityData$Age, fixed=TRUE) )
  return( MortalityData )
  
}