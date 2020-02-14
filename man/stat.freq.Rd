\name{stat.freq}
\alias{stat.freq}
\title{ Descriptive measures of grouped data }
\description{
  By this process the variance and central measures ar found: average, medium 
  and mode of grouped data.
}
\usage{
stat.freq(histogram)
}
\arguments{
  \item{histogram}{ Object create by function hist() }
}
\value{
  Statistics of grouped data.
}

\author{ Felipe de mendiburu }

\seealso{\code{\link{polygon.freq}}, \code{\link{table.freq}}, \code{\link{graph.freq}},
\code{\link{intervals.freq}}, \code{\link{sturges.freq}}, \code{\link{join.freq}},
\code{\link{ogive.freq}}, \code{\link{normal.freq} } }

\examples{
library(agricolae)
data(growth)
grouped<-with(growth,hist(height,plot=FALSE))
measures<-stat.freq(grouped)
print(measures)

}
\keyword{ univar }% at least one, from doc/KEYWORDS

