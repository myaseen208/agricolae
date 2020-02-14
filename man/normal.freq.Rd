\name{normal.freq}
\alias{normal.freq}
%- normal.freq.
\title{ Normal curve on the histogram }
\description{
  A normal distribution graph elaborated from the histogram previously
  constructed. The average and variance are obtained from the data grouped
  in the histogram.
}
\usage{
normal.freq(histogram, frequency=1, ...)
}

\arguments{
  \item{histogram}{ object constructed by the function hist}
  \item{frequency}{ 1=counts, 2=relative, 3=density }
  \item{\dots}{ Other parameters of the function hist }
}

\author{ Felipe de Mendiburu }

\seealso{\code{\link{polygon.freq}}, \code{\link{table.freq}}, \code{\link{stat.freq}},
\code{\link{intervals.freq}}, \code{\link{sturges.freq}}, \code{\link{join.freq}}, 
\code{\link{ogive.freq}}, \code{\link{graph.freq} } }

\examples{
library(agricolae)
data(growth)
#startgraph
h1<-with(growth,hist(height,col="green",xlim=c(6,14)))
normal.freq(h1,col="blue")
#endgraph
#startgraph
h2<-with(growth,graph.freq(height,col="yellow",xlim=c(6,14),frequency=2))
normal.freq(h2,frequency=2)
#endgraph
}
\keyword{ aplot }% at least one, from doc/KEYWORDS

