#' make.candle
#' 
#' This will return a single bar of all the candles that were given
#' 
#' @author Siva Sunku
#' @keywords make.candle
#' @note
#' 
#' @param  bars  - bars with which, to be evaluated
#' @export
#' 
make.candle <- function(bars){
  if (!is.xts(bars)){
    stop("In make.candle bars is not an xts object")
  }
  
  if (!is.OHLC(bars)){
    stop("In make.candle bars is not an OHLC object")
  }
  
  res <- bars[nrow(bars),]
  if (has.Vo(bars)){
    res$Volume <- sum(bars$Volume,na.rm = TRUE)
  }
  
  res$Open  <- bars[1,]$Open
  res$Low   <- min(bars$Low,na.rm = TRUE)
  res$High  <- max(bars$High,na.rm = TRUE)
  return(res)
}
