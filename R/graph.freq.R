#' Histogram
#' 
#' In many situations it has intervals of class defined with its respective
#' frequencies. By means of this function, the graphic of frequency is obtained
#' and it is possible to superpose the normal distribution, polygon of
#' frequency, Ojiva and to construct the table of complete frequency.
#' 
#' 
#' @param x a vector of values, a object hist(), graph.freq()
#' @param counts frequency and x is class intervals
#' @param breaks a vector giving the breakpoints between histogram cells
#' @param frequency 1=counts, 2=relative, 3=density
#' @param plot logic
#' @param nclass number of classes
#' @param xlab x labels
#' @param ylab y labels
#' @param las numeric in 0,1,2,3; the style of axis labels. see plot()
#' @param axes TRUE or FALSE
#' @param \dots other parameters of plot
#' 
#' @return \item{breaks}{ a vector giving the breakpoints between histogram
#' cells } \item{counts}{ frequency and x is class intervals} \item{mids}{
#' center point in class } \item{relative}{ Relative frequency, height }
#' \item{density}{ Density frequency, height }
#' @author Felipe de Mendiburu
#' @seealso \code{\link{polygon.freq}}, \code{\link{table.freq}},
#' \code{\link{stat.freq}},\code{\link{intervals.freq}},\code{\link{sturges.freq}},
#' \code{\link{join.freq}},\code{\link{ogive.freq}}, \code{\link{normal.freq} }
#' @keywords aplot
#' @export
#' @examples
#' 
#' 
#' library(agricolae)
#' data(genxenv)
#' yield <- subset(genxenv$YLD,genxenv$ENV==2)
#' yield <- round(yield,1)
#' h<- graph.freq(yield,axes=FALSE, frequency=1, ylab="frequency",col="yellow")
#' axis(1,h$breaks)
#' axis(2,seq(0,20,0.1))
#' # To reproduce histogram.
#' h1 <- graph.freq(h, col="blue", frequency=2,border="red", density=8,axes=FALSE,
#' xlab="YIELD",ylab="relative")
#' axis(1,h$breaks)
#' axis(2,seq(0,.4,0.1))
#' # summary, only frecuency
#' limits <-seq(10,40,5)
#' frequencies <-c(2,6,8,7,3,4)
#' #startgraph
#' h<-graph.freq(limits,counts=frequencies,col="bisque",xlab="Classes")
#' polygon.freq(h,col="red")
#' title( main="Histogram and polygon of frequency",
#' ylab="frequency")
#' #endgraph
#' # Statistics
#' measures<-stat.freq(h)
#' print(measures)
#' # frequency table full
#' round(table.freq(h),2)
#' #startgraph
#' # ogive
#' ogive.freq(h,col="red",type="b",ylab="Accumulated relative frequency",
#' xlab="Variable")
#' # only .frequency polygon
#' h<-graph.freq(limits,counts=frequencies,border=FALSE,col=NULL,xlab="  ",ylab="")
#' title( main="Polygon of frequency",
#' xlab="Variable", ylab="Frecuency")
#' polygon.freq(h,col="blue")
#' grid(col="brown")
#' #endgraph
#' # Draw curve for Histogram
#' h<- graph.freq(yield,axes=FALSE, frequency=3, ylab="f(yield)",col="yellow")
#' axis(1,h$breaks)
#' axis(2,seq(0,0.18,0.03),las=2)
#' lines(density(yield), col = "red", lwd = 2)
#' title("Draw curve for Histogram")
#' 
graph.freq <-
function (x, breaks=NULL,counts=NULL,frequency=1, plot=TRUE, nclass=NULL,xlab="",ylab="",axes="",las=1,...)
{
if(axes=="") ejes=TRUE
else {
ejes<-FALSE
if (axes) ejes<-TRUE
}
if (xlab=="") xlab= deparse(substitute(x))
if (is.numeric(x) & is.null(counts)) {
x<-na.omit(x)
	# histogram
if (is.null(nclass)) {
if (is.null(breaks)) {
breaks <- sturges.freq(x)$breaks
}
}
else {
breaks <- sturges.freq(x,k=nclass)$breaks
}

k<-length(breaks)
n<- length(x)
counts <- rep(0,k-1)
for (i in 1:n) {
for (j in 1:(k-2)) {
if( (x[i] >= breaks[j]) && (x[i] < breaks[j + 1])) counts[j]<-counts[j]+1
}
}
for (i in 1:n) {
	if( (x[i] >= breaks[k-1]) && (x[i] <= breaks[k])) counts[k-1]<-counts[k-1]+1
}
    k <- length(counts)
    mids <- rep(0, k)
    ancho <- rep(0, k)
    for (i in 1:k) {
        mids[i] <- (breaks[i] + breaks[i + 1])/2
        ancho[i] <- (breaks[i + 1] - breaks[i])
    }
    altura <- round(1.1 * max(counts), 0)
}
#############
else  {
if( is.list(x)) {
breaks<- x$breaks
counts <- x$counts
}
else breaks <- x
k<-length(counts)
mids<-rep(0,k)
ancho<-rep(0,k)
for (i in 1:k) {
mids[i]<-(breaks[i]+breaks[i+1])/2
ancho[i]<-(breaks[i+1]-breaks[i])
}
}
################
a<-breaks[1]-ancho[1]/2
b<-breaks[k+1]+ancho[k]/2
relative<-round(counts/sum(counts),4)
density <- relative/ancho
histogram<-structure(list(breaks=breaks,counts=counts,mids=mids,relative=relative,density=density),class="graph.freq")

if(plot) {
x <- c(a, b)
if(frequency==1)height<-round(1.1*max(counts),1)
if(frequency==2)height<-round(1.1*max(relative),4)
if(frequency==3)height<-round(1.1*max(density),4)
y <- c(0, height)
# suppressWarnings(warning(plot(x,y, type = "n", xlab=xlab,ylab=ylab,axes=axes,...)))
if(ejes){
suppressWarnings(warning(plot(x,y, type = "n", xlab=xlab,ylab=ylab,axes=FALSE,...)))
axis(1,breaks,las=las)->ax; axis(2,las=las)->ay
}
else suppressWarnings(warning(plot(x,y, type = "n", xlab=xlab,ylab=ylab,axes=axes,...)))
if (frequency==1) {
for (j in 1:k) {
suppressWarnings(warning(rect(breaks[j], 0, breaks[j + 1], counts[j], ...)))
}}
if (frequency==2) {
for (j in 1:k) {
suppressWarnings(warning(rect(breaks[j], 0, breaks[j + 1], relative[j], ...)))
}}
if (frequency==3) {
for (j in 1:k) {
suppressWarnings(warning(rect(breaks[j], 0, breaks[j + 1], density[j], ...)))
}}
}
class(histogram)<-c("graph.freq","histogram")
    invisible(histogram)
}
