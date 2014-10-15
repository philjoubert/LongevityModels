ExtractDeathsFromMortalityTable <- function( MortalityTable, Years, Ages ) {
  
  return( matrix( MortalityTable$dx[ MortalityTable$Year %in% Years & MortalityTable$Age %in% Ages ] ), 
                  ncol=length(Years) )  
  
}