#' The polygon of frequency on the histogram
#' 
#' The polygon is constructed single or on a histogram.  It is necessary to
#' execute the function previously hist.
#' 
#' 
#' @param histogram Object constructed by the function hist
#' @param frequency numeric, counts(1), relative(2) and density(3)
#' @param \dots Other parameters of the function hist
#' @author Felipe de Mendiburu Delgado
#' @seealso \code{\link{polygon.freq}}, \code{\link{table.freq}},
#' \code{\link{stat.freq}}, \code{\link{intervals.freq}},
#' \code{\link{sturges.freq}}, \code{\link{join.freq}},
#' \code{\link{graph.freq}}, \code{\link{normal.freq} }
#' @keywords aplot
#' @export
#' @examples
#' 
#' library(agricolae)
#' data(growth)
#' #startgraph
#' h1<-with(growth,hist(height,border=FALSE,xlim=c(6,14)))
#' polygon.freq(h1,frequency=1,col="red")
#' #endgraph
#' #startgraph
#' h2<-with(growth,graph.freq(height,frequency=2,col="yellow",xlim=c(6,14)))
#' polygon.freq(h2,frequency=2,col="red")
#' #endgraph
#' 
polygon.freq <-
function(histogram,frequency=1, ...){
xx<-histogram$mids
zz<-histogram$breaks
if(frequency==1)yy<-histogram$counts
if(frequency==2)yy<-histogram$counts/sum(histogram$counts)
if(frequency==3)yy<-histogram$density
x1 <- xx[1]-zz[2]+zz[1]
z<-length(zz)
x2<-xx[z-1]+zz[z]-zz[z-1]
xx<-c(x1,xx,x2)
yy<-c(0,yy,0)
lines(xx,yy, ...)
abline(h=0)
}
