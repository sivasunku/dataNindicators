#'
#' This will apply higher interval values to lower inverval datasets.
#' 
#' @description - Apply higher interval values to lower values. e.g. If weekly indicator need to be applied to all daily rows for that
#' particular week. 
#' @param l - lower ordered xts
#' @param h - higher ordered xts
#' @param parms - which all the columns to be applied from higher order to lower order. Default All
#' @return higher ordered xts distributed across to all lower ordered ones. only index of lowerorder is used
#' @export
#' @rdname revapply
#' 
revapply <- function(l,h,parms = NULL){
  #Make a new table with start and end dates of larger time series
  t       <- xts(order.by = index(h))
  t$start <- as.numeric( as.POSIXct(index(h)) )
  t$end   <- lag.xts(t$start,k = -1)
  
  #when lagged, last entry will be NA. Add the difference unit to last row
  diff <- as.numeric(t[nrow(t)-1,]$end  - t[nrow(t)-1,]$start)
  t[nrow(t),]$end <- t[nrow(t),]$start + diff
  
  if (missing(parms)){
    parms <- c(colnames(h))
  }
  
  res <- xts(order.by = index(l),tzone = Sys.timezone())
  for (i in 1:length(parms)){
    res <- merge(res,NA)
  }
  colnames(res) <- parms
  
  maxWklyrow <- nrow(h)
  maxDlyrow  <- nrow(l)
  wklyRow    <- 1
  dlyRow     <- 1
  
  for(wklyRow in 1:maxWklyrow){
    st <- as.numeric(t[wklyRow,]$start)
    en <- as.numeric(t[wklyRow,]$end)
    dt <- as.numeric( as.POSIXct(index(d[dlyRow,])) )
    if ((dt >= st )  && (dt < en) ){
      res[dlyRow,parms] <- w[wklyRow,parms]
    }
    while ( (dlyRow < maxDlyrow) && (dt < en) ){
      dlyRow <- dlyRow + 1
      dt <- as.numeric(as.POSIXct(index(d[dlyRow,])))
      if ((dt >= st )  && (dt < en) ){
        res[dlyRow,parms] <- h[wklyRow,parms]
      }
    }
  }
  return(res)
}