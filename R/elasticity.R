#' Calculate the elders Ray of given OHLCV
#'
#' The function returns the elders Ray for a given OHLCV
#' @param m - OHLC
#' @param FUN - Which price is OHLC to be used for calculating. Default Cl. If 
#' If Hi to be used, use FUN - Hi
#' @return returns xts object with the elasticity
#' @description elders Ray gives two units bullPower, bearPower. 
#' @description bullPower - Difference from High to EMA(n)
#' @description bearPower - Difference from Low to EMA(n)
#' @author Siva Sunku
#' @keywords elders Ray
#' @examples
#' elderRay(m,n=13)
#' @export
#' 
elasticity <- function(m,FUN=Cl){

  if (!is.xts(m) || !is.OHLCV(m)){
    stop("elasticity can be calculated on xts variables & OHLCV")
  }
  
  round( ( ( FUN(m) / lag.xts(FUN(m) ) ) - 1 ) / log(Vo(m)) , digits = 4 )

}