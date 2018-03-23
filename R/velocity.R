#' Calculates the velocity & acceleration in a given n points.
#'
#' The function returns the slope of given n points
#' @param n - input numbers
#' @param pct - If 'n' should be normalized(to 100s) before finding the slope. 
#' @return returns velocity & acceleration
#' @author Siva Sunku
#' @keywords velocity
#' @examples
#' velocity(n,pct = TRUE)
#' @export

velocity <- function(n,pct=FALSE){
  if (!is.numeric(n)){
    stop("velocity n is non-numeric")
  }
  
  if ( !is.null(ncol(n)) && (ncol(n) > 1) ){
    stop("velocity can be applicable for univariate sequence")
  }
  
  if (pct){
    m <- mean(n)
    n <- (n/m)*100
  }
  
  vel <- diff(n)
  acc <- diff(vel)
  res <- cbind(vel,acc)
  colnames(res) <- c("vel","acc")
  return(res)
}
