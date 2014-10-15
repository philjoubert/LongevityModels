ExtractExposuresFromMortalityTable <- function( MortalityTable, Years, Ages ) {
  
  return( matrix( MortalityTable$lx[ MortalityTable$Year %in% Years & MortalityTable$Age %in% Ages ] ), 
          ncol=length(Years) )  
  
}