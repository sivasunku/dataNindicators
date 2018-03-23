#' Calculate the slope of given n points
#'
#' The function returns the slope of given n points
#' @param n - input numbers
#' @param pct - If 'n' should be normalized before finding the slope. 
#' @return returns slope
#' @author Siva Sunku
#' @keywords slope
#' @examples
#' slope(n=c(10,20,30))
#' @export

slope <- function(n,pct=FALSE){
  if (!is.numeric(n)){
    stop("slope n is non-numeric")
  }
  
  if ( !is.null(ncol(n)) && (ncol(n) > 1) ){
    stop("slope can be applicable for univariate sequence")
  }
  
  n <- as.numeric(n)
  if (pct){
    m <- mean(n)
    n <- (n/m)*100
  }
  
  l <- 1:length(n)
  
  fit <- lm(n~l)
  return(round(as.numeric(fit$coefficients[2]),digits = 2))
  
}
