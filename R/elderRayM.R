#' EldersRay Modified. Calculates the eldersRay & for a given 'N' 
#'
#' The function returns the elders Ray for a given OHLCV
#' @param m - OHLC
#' @param eman - n for calculating EMA
#' @param pown - n for calculating the bull/bear
#' @return po
#' @description 
#' elders Ray gives two units bullPower, bearPower. 
#' @description bullPower - Difference from High to EMA(n)
#' @description bearPower - Difference from Low to EMA(n)
#' @author Siva Sunku
#' @keywords elders Ray
#' @examples
#' elderRay(m,n=13)
#' @export
#' 

elderRay <- function(m,n=13){
  if (!is.OHLC(m)){
    stop("elder Ray m should be OHLCV")
  }
  bull <- Hi(m) - EMA(Cl(m),n = n)
  bear <- Lo(m) - EMA(Cl(m),n = n)
  res    <- merge.xts(bull,bear)
  colnames(res) <- c("bullPower","bearPower")
  return(res)
}