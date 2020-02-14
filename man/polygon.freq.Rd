\name{polygon.freq}
\alias{polygon.freq}
%- polygon.freq.
\title{ The polygon of frequency on the histogram }
\description{
  The polygon is constructed single or on a histogram.
  It is necessary to execute the function previously hist.
}
\usage{
polygon.freq(histogram, frequency=1, ...)
}

\arguments{
  \item{histogram}{ Object constructed by the function hist }
  \item{frequency}{ numeric, counts(1), relative(2) and density(3) }
  \item{\dots}{ Other parameters of the function hist }
}

\author{ Felipe de Mendiburu Delgado }

\seealso{\code{\link{polygon.freq}}, \code{\link{table.freq}}, \code{\link{stat.freq}},
\code{\link{intervals.freq}}, \code{\link{sturges.freq}}, \code{\link{join.freq}},
\code{\link{graph.freq}}, \code{\link{normal.freq} } }

\examples{
library(agricolae)
data(growth)
#startgraph
h1<-with(growth,hist(height,border=FALSE,xlim=c(6,14)))
polygon.freq(h1,frequency=1,col="red")
#endgraph
#startgraph
h2<-with(growth,graph.freq(height,frequency=2,col="yellow",xlim=c(6,14)))
polygon.freq(h2,frequency=2,col="red")
#endgraph
}
\keyword{ aplot }% at least one, from doc/KEYWORDS



