#' Setting the last character of a chain
#' 
#' A special function for the group of treatments in the multiple comparison
#' tests.  Use plot.group.
#' 
#' 
#' @param x letters
#' @return \item{x }{Returns the last character of a string}
#' @author Felipe de Mendiburu
#' @seealso \code{\link{plot.group} }
#' @keywords manip
#' @export
#' @examples
#' 
#' library(agricolae)
#' x<-c("a","ab","b","c","cd")
#' lastC(x)
#' # "a" "b" "b" "c" "d"
#' 
#' 
lastC <-
function(x) {
y<-sub(" +$", "",x)
p1<-nchar(y)
cc<-substr(y,p1,p1)
return(cc)
}
