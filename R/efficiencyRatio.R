#' Calculate the efficiencyRatio
#'
#' The function returns the efficiency Ratio for a given n-points
#' @param m - input numbers
#' @return returns the efficiency Ratio
#' @description efficiency Ratio is a trend strength measure 
#' @description It is calculated by net displacement/abs(each displacement) in a given period
#' @author Siva Sunku
#' @keywords efficiency Ratio
#' @export
#' 
efficiencyRatio <- function(m){
  nC <- m[length(m)] - m[1]
  return( nC / sum( abs( diff(m) ) ) )
}

