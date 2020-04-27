#' Finding the skewness coefficient
#' 
#' It returns the skewness of a distribution. It is similar to SAS.
#' 
#' 
#' @param x a numeric vector
#' @return The skewness of x.
#' @seealso \code{\link{kurtosis} }
#' @keywords univar
#' @importFrom stats var
#' @export
#' @examples
#' 
#' library(agricolae)
#' x<-c(3,4,5,2,3,4,NA,5,6,4,7)
#' skewness(x)
#' # value is 0,3595431, is slightly asimetrica (positive) to the right
#' 
skewness <-
function(x) {
x <- na.omit(x)
n<-length(x)
s<-sqrt(var(x))
suma<-sum((x-mean(x))^3)/s^3
k <- n*suma/((n-1)*(n-2))
return(k)
}
