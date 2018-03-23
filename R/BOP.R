#' BOP
#' 
#' This is balance of power. Basically, it shows where the price closed with respect to the length of the candle
#' 
#' @author Siva Sunku
#' @keywords bhavcopy
#' @param  bar  - bars with which, to be evaluated
#' @export
BOP <- function(bar){
  if (!is.xts(bar)){
    stop("In is.price.hit bars is not an xts object")
  }
  
  if (!is.OHLC(bar)){
    stop("In is.price.hit bars is not an OHLC object")
  }
  length <- bar$High - bar$Low
  wick   <- bar$Close - bar$Low
  
  return( round(wick / length, digits = 2) )
}