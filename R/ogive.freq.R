#' Plotting the ogive from a histogram
#' 
#' It plots the cumulative relative .frequencies with the intervals of classes
#' defined in the histogram.
#' 
#' 
#' @param histogram object created by the function hist() or graph.freq()
#' @param type what type of plot should be drawn. See plot()
#' @param xlab x labels
#' @param ylab y labels
#' @param axes TRUE or FALSE
#' @param las numeric in 0,1,2,3; the style of axis labels. see plot()
#' @param \dots Parameters of the plot()
#' @return Ogive points.
#' @author Felipe de Mendiburu
#' @seealso \code{\link{polygon.freq}}, \code{\link{table.freq}},
#' \code{\link{stat.freq}}, \code{\link{intervals.freq}},
#' \code{\link{sturges.freq}}, \code{\link{join.freq}},
#' \code{\link{graph.freq}}, \code{\link{normal.freq} }
#' @keywords aplot
#' @importFrom graphics axis
#' @export
#' @examples
#' 
#' library(agricolae)
#' data(growth)
#' h<-graph.freq(growth$height,plot=FALSE)
#' points<-ogive.freq(h,col="red",frame=FALSE,
#' xlab="Height", ylab="Accumulated relative frequency", main="ogive")
#' plot(points,type="b",pch=16,las=1,bty="l")
#' 
ogive.freq <-
function(histogram,type="",xlab="",ylab="",axes="",las=1,...)
{
if(axes=="") ejes=TRUE
else {
ejes<-FALSE
if (axes) ejes<-TRUE
}
if(type=="") type<-"b"
#if(las=="") las<-1
    yy <- histogram$counts
    zz <- histogram$breaks
    y1 <- sum(yy)
    nx <- length(yy)
    zz <-c(zz,2*zz[nx+1]-zz[nx])
    nz<-length(zz)
    y<-rep(0,nz)
    for (i in 1:nx) {
        y[i+1] <- y[i] + yy[i]/y1
    }
    y[nz]<-1
probability<-y
if(ejes){
plot(zz,y,type=type,xlab=xlab,ylab=ylab,axes=FALSE,...)
axis(1,zz,las=las)->ax; axis(2,las=las)->ay
abline(v=ax,h=ay,lty=2,col="grey")
}
else plot(zz,y,type=type,xlab=xlab,ylab=ylab,axes=axes,...)
table<-data.frame(x=zz,RCF=round(y,4))
if(xlab=="")xlab<-"x"
names(table)[1]<-xlab
return(table)
}
