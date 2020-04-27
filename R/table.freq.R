#' frequency Table of a Histogram
#' 
#' It finds the absolute, relative and accumulated frequencies with the class
#' intervals defined from a previously calculated histogram by the "hist" of R
#' function.
#' 
#' 
#' @param object Object by function graph.freq()
#' @return Frequency table.\cr \item{Lower }{Lower limit class} \item{Upper
#' }{Upper limit class} \item{Main }{class point} \item{Frequency }{Frequency}
#' \item{Percentage }{Percentage frequency} \item{CF }{Cumulative frequency}
#' \item{CPF }{Cumulative Percentage frequency}
#' @author Felipe de Mendiburu
#' @seealso \code{\link{polygon.freq}}, \code{\link{stat.freq}},
#' \code{\link{graph.freq}}, \code{\link{intervals.freq}},
#' \code{\link{sturges.freq}}, \code{\link{join.freq}},
#' \code{\link{ogive.freq}}, \code{\link{normal.freq} }
#' @keywords distribution
#' @export
#' @examples
#' 
#' library(agricolae)
#' data(growth)
#' h2<-with(growth,graph.freq(height,plot=FALSE))
#' print(table.freq(h2),row.names=FALSE)
#' 
table.freq <-
function(object){
xx<-object$mids
yy<-object$counts
y1<-sum(yy)
zz<-object$breaks
x<-length(xx)
acum<-0
z<-rep(0,7*x)
dim(z)<-c(x,7)
for (i in 1:x) {
z[i,1]<-zz[i]
z[i,2]<-zz[i+1]
z[i,3]<-xx[i]
z[i,4]<-yy[i]
z[i,5]<-round(yy[i]*100/y1,1)
z[i,6]<-yy[i]+acum
acum<-z[i,6]
z[i,7]<-round(z[i,6]*100/y1,1)
}
z[nrow(z),7]<-100
colnames(z)<-c("Lower","Upper","Main","Frequency","Percentage","CF","CPF")
z<-as.data.frame(z)
invisible(z)
}
