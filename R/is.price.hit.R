#' is.price.hit
#' 
#' This will return if the given price is hit by the candlebar.
#' 
#' @author Siva Sunku
#' @keywords price
#' @note
#' 
#' @param  price - price, that needs to be evaluated
#' @param  bars  - bars with which, to be evaluated
#' @param  closeLag - if only close of previous bar to be evaluated. In this, only latest candle row is considered
#' @export

is.price.hit <- function(price, bars, closeLag = FALSE){
  if (!is.xts(bars)){
    stop("In is.price.hit bars is not an xts object")
  }
  
  if (!is.OHLC(bars)){
    stop("In is.price.hit bars is not an OHLC object")
  }
  
  if (nrow(bars) > 1) {
    bars <- make.candle(bars)
  }
  
  if ( (price >= bars$Low)  && (price <= bars$High) ){
    return (TRUE)
  }
  
  return(FALSE)
  
}

