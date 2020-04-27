#' Class intervals
#' 
#' List class intervals.
#' 
#' 
#' @param x class graph.freq, histogram or numeric
#' @return It show interval classes.
#' @author Felipe de Mendiburu
#' @seealso \code{\link{polygon.freq}}, \code{\link{table.freq}},
#' \code{\link{stat.freq}}, \code{\link{graph.freq}},
#' \code{\link{sturges.freq}}, \code{\link{join.freq}},
#' \code{\link{ogive.freq}}, \code{\link{normal.freq} }
#' @keywords univar
#' @export
#' @examples
#' 
#' library(agricolae)
#' # example 1
#' data(growth)
#' h<-hist(growth$height,plot=FALSE)
#' intervals.freq(h)
#' # example 2
#' x<-seq(10,40,5)
#' y<-c(2,6,8,7,3,4)
#' intervals.freq(x)
#' histogram <- graph.freq(x,counts=y)
#' 
intervals.freq <-
function(x){
if(class(x)[1]=="graph.freq" | class(x)[1]=="histogram")breaks<-x$breaks
if(class(x)[1]=="numeric" | class(x)[1]=="integer"  )breaks<-x
n<-length(breaks)-1
classes<-rep(0,2*n)
dim(classes)<-c(n,2)
for (i in 1:n) {
classes[i,1]<-breaks[i]
classes[i,2]<-breaks[i+1]  
}
colnames(classes)<-c("lower","upper")
return(classes)
}
