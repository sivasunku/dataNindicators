#' Calculate the bull/bear strength of a given xts OHLC
#'
#' The function returns the bull/bear strength (zero if negative)
#' @param m - OHLC
#' @param pct - If 'm' should be normalized before finding the strength
#' @return returns bullStrength,bearStrength
#' @author Siva Sunku
#' @keywords bullbearstrength
#' @export

bullbearstrength <- function(m,pct=TRUE){
  if (!is.xts(m) ){
    stop("bullBearStrength can be calculated on xts variables")
  }
  if (!is.OHLC(m) ){
    stop("bullBearStrength : m should be OHLCV")
  }
  
  m$pC <- lag.xts(Cl(m))
  m <- na.trim(m)
  
  if (pct){
    div <- apply(m,1,min)
    m$Open <- Op(m)/div * 100
    m$High <- Hi(m)/div * 100
    m$Close <- Cl(m)/div * 100
    m$pC    <- m$pC/div * 100
    m$Low  <-  Lo(m)/div * 100
  }
  
  bull <- Hi(m) - m$pC
  bull <- ifelse(bull < 0,0,bull)
  
  bear <- Lo(m) - m$pC
  bear <- ifelse(bear > 0,0,abs(bear))

  res <- merge.xts(bull,bear)
  colnames(res) <- c("bullStrength","bearStrength")
  res$netStrength <- res$bullStrength - res$bearStrength
  return(round(res,digits = 3))
}

