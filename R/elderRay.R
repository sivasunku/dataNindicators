#' Calculate the elders Ray of given OHLCV
#'
#' The function returns the elders Ray for a given OHLCV
#' @param m - OHLC
#' @param n - number of units for calculating EMA, (default - 13)
#' @param FUN - function to be applied for averges. Default EMA
#' @return returns slope
#' @description elders Ray gives two units bullPower, bearPower. 
#' @description bullPower - Difference from High to EMA(n)
#' @description bearPower - Difference from Low to EMA(n)
#' @author Siva Sunku
#' @keywords elders Ray
#' @examples
#' elderRay(m,n=13)
#' @export
#' 

elderRay <- function(m,n=13,FUN=EMA){
  if (!is.OHLC(m)){
    stop("elder Ray m should be OHLCV")
  }
  bull <- Hi(m) - FUN(Cl(m),n = n)
  bear <- Lo(m) - FUN(Cl(m),n = n)
  res    <- merge.xts(bull,bear)
  colnames(res) <- c("bullPower","bearPower")
  return(res)
}