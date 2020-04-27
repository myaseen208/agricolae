#' Finding the Kurtosis coefficient
#' 
#' It obtains the value of the kurtosis for a normally distributed variable.
#' The result is similar to SAS.
#' 
#' 
#' @param x a numeric vector
#' @return
#' 
#' \item{x }{ The kurtosis of x }
#' @seealso \code{\link{skewness} }
#' @keywords univar
#' @export
#' @examples
#' 
#' library(agricolae)
#' x<-c(3,4,5,2,3,4,5,6,4,NA,7)
#' kurtosis(x)
#' # value is -0.1517996
#' 
kurtosis <-
function(x) {
x<-na.omit(x)
n<-length(x)
suma<-sum((x-mean(x))^4)/(var(x))^2
k <- n*(n+1)*suma/((n-1)*(n-2)*(n-3)) - 3*(n-1)^2/((n-2)*(n-3))
return(k)
}
