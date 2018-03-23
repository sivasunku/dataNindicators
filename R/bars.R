#' Bars - Functions related to all the bars/candles
#' 
#' Functions related to all the bars/candles
#'
#' @author Siva Sunku
#' @keywords bars
#' @note
#'
#' @param b - bar/OHLC, 
#' @param pct - percentage of points considered in calculation
#' @param price - price
#' @param dir - direction (GREEN/LONG stands for 1, SHORT/RED stands for -1)
#' 
#' @details bar - converts OHLC object into bar class. Both are aliases infact
#' @return TRUE/FALSE
#' @rdname bars
#' @export
bar <- function(b){
  if ( !is.OHLC(b) ){  
    stop(" in bars b is not an OHLC")  
  }
  res <- OHLCV(b)
  class(res) <- append("bar",class(res))
  return (res)
}

#' @details is.bar - checks if given object is a bar
#' @rdname bars
#' @export
is.bar <- function(b){
  return ( inherits(t,"bars") )
}

#' @details isgreen.bar - evaluates if close of a bar is atleast percent greater than open
#' @return isgreen.bar - returns TRUE/FALSE
#' @rdname bars
#' @export
isgreen.bar <- function(b,pct = 0){
  if ( !is.OHLC(b) ){  
    stop(" in bar.isgreen b is not an OHLC")  
  }
  b <- OHLC(b)
  d <- Op(b) * pct / 100
  return( Cl(b) > ( Op(b) + d ) )
}

#' @details isred.bar - evaluates if close of a bar is atleast percent lessr than open
#' @return isred.bar - returns TRUE/FALSE
#' @rdname bars
#' @export
isred.bar <- function(b,pct = 0){
  if ( !is.OHLC(b) ){  
    stop(" in bar.isred b is not an OHLC")  
  }
  b <- OHLC(b)
  d <- Op(b) * pct / 100
  return( ( Cl(b) + d ) < Op(b) )
}

#' @details isdoji.bar - evaluates if close and open are near to each other by pct
#' @return isdoji.bar - returns TRUE/FALSE
#' @rdname bars
#' @export
isdoji.bar <- function(b,pct = 0){
  if ( !is.OHLC(b) ){  
    stop(" in bar.isdoji b is not an OHLC")  
  }
  
  b <- OHLC(b)
  d <- Op(b) * pct / 100
  return ( abs( Cl(b) - Op(b) ) <= d )
}

#' @details is.pricehit.bar - evaluates if the price 'p' is hit by bar 'b'
#' @return is.pricehit.bar  - returns TRUE/FALSE
#' @rdname bars
#' @export
is.pricehit.bar <- function(b,price){
  
  if ( !is.bar(b) && !is.OHLC(b) ){
    stop("is.pricehit.bar - b is not a bar")
  }
  
  l <- as.double(Lo(c))
  h <- as.double(Hi(c))
  return ( (price <= h) && (price >= l) )
}

#' @details is.pricexed.bar - evaluates if the bar 'b' closed on price 'p' in given direction 'dir'.
#' if direction is - LONG/GREEN(1) - b closes > p , returns TRUE
#' @return is.pricexed.bar  - returns TRUE/FALSE
#' @rdname bars
#' @export
is.pricexed.bar <- function(b,price,dir = 1){
  if ( !is.bar(b) && !is.OHLC(b) ){
    stop("is.pricexed.bar - b is not a bar")
  }

  c <- OHLC(b)
  l <- min(b)
  h <- max(b)
  
  if ( (l<=price) && (h>=price) ){
    if ( (dir == LONG)  && (Cl(c) >= price) ) { return (TRUE) }
    if ( (dir == SHORT) && (Cl(c) <= price) ) { return (TRUE)  }
  }
  return(FALSE)
}
