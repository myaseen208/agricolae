\name{join.freq}
\alias{join.freq}
%- join.freq.
\title{ Join class for histogram }
\description{
  In many situations it is required to join classes because of the low .frequency
  in the intervals. In this process, it is required to join the intervals and
  ad the .frequencies of them.
 }
\usage{
join.freq(histogram, join)
}

\arguments{
  \item{histogram}{ Class graph.freq }
  \item{join}{ vector }
}
\value{
 New histogram with union of classes.
}
\author{ Felipe de Mendiburu }
 
\seealso{\code{\link{polygon.freq}}, \code{\link{table.freq}}, \code{\link{stat.freq}},
\code{\link{intervals.freq}}, \code{\link{sturges.freq}}, \code{\link{graph.freq}},
\code{\link{ogive.freq}}, \code{\link{normal.freq} } }
 
\examples{
library(agricolae)
data(natives)
# histogram
h1<-graph.freq(natives$size,plot=FALSE)
round(table.freq(h1),4)
# Join classes  9, 10,11 and 12 with little frequency.
h2<-join.freq(h1,9:12)
# new table
plot(h2,col="bisque",xlab="Size")
round(summary(h2),4)
}
\keyword{ univar }% at least one, from doc/KEYWORDS

