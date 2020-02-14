\name{intervals.freq}
\alias{intervals.freq}
%- intervals.freq.
\title{ Class intervals }
\description{
  List class intervals.
}
\usage{
intervals.freq(x)
}

\arguments{
  \item{x}{ class graph.freq, histogram or numeric }
}
\value{
It show interval classes.
}
\author{ Felipe de Mendiburu }
\seealso{\code{\link{polygon.freq}}, \code{\link{table.freq}}, \code{\link{stat.freq}},
\code{\link{graph.freq}}, \code{\link{sturges.freq}}, \code{\link{join.freq}}, 
\code{\link{ogive.freq}}, \code{\link{normal.freq} } }

\examples{
library(agricolae)
# example 1
data(growth)
h<-hist(growth$height,plot=FALSE)
intervals.freq(h)
# example 2
x<-seq(10,40,5)
y<-c(2,6,8,7,3,4)
intervals.freq(x)
histogram <- graph.freq(x,counts=y)
}
\keyword{ univar }% at least one, from doc/KEYWORDS

