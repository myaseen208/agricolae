\name{sturges.freq}
\alias{sturges.freq}
%- sturges.freq.
\title{ Class intervals for a histogram, the rule of Sturges }
\description{
  if k=0 then classes: k = 1 + log(n,2).
  if k > 0, fixed nclass.
}
\usage{
sturges.freq(x,k=0)
}
\arguments{
  \item{x}{ vector }
  \item{k}{ constant }
  }

\value{
Statistics of sturges for a histogram.
}
\references{
  Reza A. Hoshmand. 1988. Statistical Methods for Agricultural Sciences,
Timber Press, Incorporated, pag 18-21.
}
\author{ Felipe de mendiburu }

\seealso{\code{\link{polygon.freq}}, \code{\link{table.freq}}, \code{\link{stat.freq}},
\code{\link{intervals.freq}}, \code{\link{graph.freq}}, \code{\link{join.freq}},
\code{\link{ogive.freq}}, \code{\link{normal.freq} } }

\examples{
library(agricolae)
data(natives)
classes<-with(natives,sturges.freq(size))
# information of the classes
breaks <- classes$breaks
breaks
#startgraph
# Histogram with the established classes
h<-with(natives,graph.freq(size,breaks,frequency=1, col="yellow",axes=FALSE,
    xlim=c(0,0.12),main="",xlab="",ylab=""))
axis(1,breaks,las=2)
axis(2,seq(0,400,50),las=2)
title(main="Histogram of frequency\nSize of the tubercule of the Oca",
xlab="Size of the oca", ylab="Frequency")
#endgraph
}
\keyword{ manip }% at least one, from doc/KEYWORDS

