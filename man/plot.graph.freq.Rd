\name{plot.graph.freq}
\alias{plot.graph.freq}
%- plot.graph.freq.
\title{ Histogram }
\description{
  In many situations it has intervals of class defined with its respective
  frequencies. By means of this function, the graphic of frequency is obtained 
  and it is possible to superpose the normal distribution, polygon of frequency, 
  Ojiva and to construct the table of complete frequency.
}
\usage{
\method{plot}{graph.freq}(x, breaks=NULL,counts=NULL,frequency=1,plot=TRUE,
nclass=NULL,xlab="",ylab="",axes = "",las=1,...)
}
\arguments{
  \item{x}{ a vector of values, a object hist(), graphFreq() }
  \item{counts}{ frequency and x is class intervals}
  \item{breaks}{ a vector giving the breakpoints between histogram cells }
  \item{frequency}{ 1=counts, 2=relative, 3=density }
  \item{plot}{ logic }
  \item{nclass}{ number of classes }
  \item{xlab}{ x labels}
  \item{ylab}{ y labels} 
  \item{axes}{ TRUE or FALSE}
  \item{las}{ numeric in {0,1,2,3}; the style of axis labels. see plot()}     
  \item{\dots}{ other parameters of plot}
}

\value{
  \item{breaks}{ a vector giving the breakpoints between histogram cells }
  \item{counts}{ frequency and x is class intervals}
  \item{mids}{ center point in class }
  \item{relative}{ Relative frequency, height }
  \item{density}{ Density frequency, height  } 
}

\author{ Felipe de Mendiburu }

\seealso{\code{\link{polygon.freq}}, \code{\link{table.freq}}, 
\code{\link{stat.freq}},\code{\link{intervals.freq}},\code{\link{sturges.freq}},
\code{\link{join.freq}},\code{\link{ogive.freq}}, \code{\link{normal.freq} } }

\examples{

library(agricolae)
data(genxenv)
yield <- subset(genxenv$YLD,genxenv$ENV==2)
yield <- round(yield,1)
h<- graph.freq(yield,axes=FALSE, frequency=1, ylab="frequency",col="yellow")
axis(1,h$breaks)
axis(2,seq(0,20,0.1))
# To reproduce histogram.
h1 <- plot(h, col="blue", frequency=2,border="red", density=8,axes=FALSE, 
xlab="YIELD",ylab="relative")
axis(1,h$breaks)
axis(2,seq(0,.4,0.1))
# summary, only frecuency
limits <-seq(10,40,5)
frequencies <-c(2,6,8,7,3,4)
#startgraph
h<-graph.freq(limits,counts=frequencies,col="bisque",xlab="Classes")
polygon.freq(h,col="red")
title( main="Histogram and polygon of frequency",
ylab=".frequency")
#endgraph
# Statistics
measures<-stat.freq(h)
print(measures)
# frequency table full
round(table.freq(h),2)
#startgraph
# ogive
ogive.freq(h,col="red",type="b",ylab="Accumulated relative frequency",
xlab="Variable")
# only frequency polygon
h<-graph.freq(limits,counts=frequencies,border=FALSE,col=NULL,xlab="  ",ylab="")
title( main="Polygon of frequency",
xlab="Variable", ylab="Frecuency")
polygon.freq(h,col="blue")
grid(col="brown")
#endgraph
# Draw curve for Histogram
h<- graph.freq(yield,axes=FALSE, frequency=3, ylab="f(yield)",col="yellow")
axis(1,h$breaks)
axis(2,seq(0,0.18,0.03),las=2)
lines(density(yield), col = "red", lwd = 2)
title("Draw curve for Histogram")
}
\keyword{ aplot }% at least one, from doc/KEYWORDS

