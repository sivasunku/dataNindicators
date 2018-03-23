#' Calculate the flow and returns the 
#'
#' The function returns the difference of Highs and lows
#' If Candle is Green (Closed above previous Close) - Difference is - ( Close minus trueLow)
#' If Candle is Red   (Closed below previous Close) - Difference is - ( trueHigh minus Close)
#' trueHigh - Max of CurrentHigh,PrevClose
#' trueLow  - Min of CurrentLow, prevClose
#' @param m - OHLC
#' @return returns the bearishFlow,bullishFlow
#' @author Siva Sunku
#' @keywords flowDex
#' @export
flow <- function(m){
  GREEN <- 1
  RED <- -1
  if (!is.xts(m) || !is.HLC(m)){
    stop("flowDex can be calculated on xts variables & OHLCV")
  }
  HLC <- HLC(m)
  m$closeLag <- lag.xts(HLC[, 3])
  m$trueHigh <- pmax(HLC[, 1], m$closeLag, na.rm = FALSE)
  m$trueLow  <- pmin(HLC[, 2], m$closeLag, na.rm = FALSE)
  #1- Green : -1 - Red
  m$color <- ifelse( (HLC[,3] > m$closeLag),GREEN,RED)
  
  #GreenMove - If green color, trueLow to close is considered as move
  #RedMove   - If Red color,  trueHigh minus Close is the move
  m$flow <- ifelse(m$color==GREEN,m$Close - m$trueLow,m$trueHigh - m$Close )
  return(m[,c("color","flow")])
}

#' Calculates the sum/mean of flow and returns the value
#'
#' @param m - xts
#' @param fFUN - function to be applied. like sum/mean
#' @return returns the bearishFlow,bullishFlow
#' @author Siva Sunku
#' @keywords flowDex
#' @export
flowDex <- function(m,fFUN = mean){
  GREEN <-  1
  RED   <- -1

  if (!is.xts(m)){
    stop("flowDex can be calculated on xts variables & OHLCV")
  }
  
  d <- m[,c("color","flow")]
  a <- as.numeric(ifelse(d$color == GREEN,d$flow,NA))
  bullFlow <- fFUN(a,na.rm = TRUE)
  b <- as.numeric(ifelse(d$color == RED,d$flow,NA))
  bearFlow <- fFUN(b,na.rm = TRUE)
  c("bullFlow" = bullFlow,"bearFlow" = bearFlow)
}
