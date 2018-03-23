#' Calculate the various pivots
#'
#' The function returns the xts object with r1..s3 pivots.
#' @param m - the OHLC xts object
#' @param type - type of the pivots to be calculated. default regular pivots.
#' @return returns OHLC xts with pivots. If nothing matches to type, returns the xts object itself.
#' @author Siva Sunku
#' @keywords pivots
#' @examples
#' pivots(xts, type="woodies")
#' @export


pivots <- function(m,type=c("regular","woodies","camarilla")){
  type <- match.arg(type)
  if (!is.xts(m) ){
    stop("woodiesPivots can be calculated on xts variables")
  }
  if (!is.OHLC(m) ){
    stop("woodiesPivots : m should be OHLCV")
  }

  temp <- merge.xts(Op(m),lag.xts(HLC(m)))

  if ( type == "woodies"){
    res <- round( ( Hi(temp) + Lo(temp) + (2*Op(temp)) ) / 4, digits = 2 )
    colnames(res) <- c("pivot")

    res$r1    <- ( 2 * res$pivot ) - Lo(temp)
    res$s1    <- ( 2 * res$pivot ) - Hi(temp)

    res$r2    <- res$pivot + ( Hi(temp) - Lo(temp) )
    res$s2    <- res$pivot - ( Hi(temp) - Lo(temp) )

    res$r3    <- Hi(temp) + 2 * (res$pivot - Lo(temp))
    res$s3    <- Lo(temp) - 2 * (Hi(temp) - res$pivot)
    return(res)
  }
  else if (type == "camarilla") {

    res <- Hi(temp) - Lo(temp)
    colnames(res) <- c("range")
    res$pivot <- round( ( Hi(temp) + Lo(temp) + Cl(temp) ) / 3, digits = 2 )


    res$s1    <- Cl(temp) - res$range * 1.1/12
    res$s2    <- Cl(temp) - res$range * 1.1/6
    res$s3    <- Cl(temp) - res$range * 1.1/4
    res$s4    <- Cl(temp) - res$range * 1.1/2

    res$r1    <- Cl(temp) + res$range * 1.1/12
    res$r2    <- Cl(temp) + res$range * 1.1/6
    res$r3    <- Cl(temp) + res$range * 1.1/4
    res$r4    <- Cl(temp) + res$range * 1.1/2

    return(temp)
  }
  else if (type == "regular"){
    res$pivot <- round( ( Hi(temp) + Lo(temp) + Cl(temp) ) / 3, digits = 2 )

    res$s1    <- ( 2 * res$pivot ) - Hi(temp)
    res$r1    <- ( 2 * res$pivot ) - Lo(temp)

    res$s2    <- res$pivot - ( Hi(temp) - Lo(temp) )
    res$r2    <- res$pivot + ( Hi(temp) - Lo(temp) )

    res$s3    <- ( 2 * res$pivot ) - ( (2 * Hi(temp)) - Lo(temp) )
    res$r3    <- ( 2 * res$pivot ) - ( Hi(temp) - ( 2* Lo(temp) ) )

    return(res)
  }

  return(m)
}
