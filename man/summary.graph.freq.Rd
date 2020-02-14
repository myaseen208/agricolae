\name{summary.graph.freq}
\alias{summary.graph.freq}
%- summary.graph.freq.
\title{ frequency Table of a Histogram  }
\description{
  It finds the absolute, relative and accumulated frequencies with the class 
  intervals defined from a previously calculated histogram by the "hist" of R
  function.
}
\usage{
\method{summary}{graph.freq}(object,...)
}

\arguments{
  \item{object}{ Object by function graph.freq() }
  \item{\dots}{ other parameters of graphic}
}
\value{
Frequency table.\cr
  \item{Lower }{Lower limit class}
  \item{Upper }{Upper limit class}
  \item{Main }{class point}
  \item{Frequency }{Frequency}
  \item{Percentage }{Percentage frequency}
  \item{CF }{Cumulative frequency}
  \item{CPF }{Cumulative Percentage frequency}
}
\author{ Felipe de Mendiburu }

\seealso{\code{\link{polygon.freq}}, \code{\link{stat.freq}}, \code{\link{graph.freq}},
\code{\link{intervals.freq}}, \code{\link{sturges.freq}}, \code{\link{join.freq}},
\code{\link{ogive.freq}}, \code{\link{normal.freq} } }          
\examples{
library(agricolae)
data(growth)
h2<-with(growth,graph.freq(height,plot=FALSE))
print(summary(h2),row.names=FALSE)
}
\keyword{ distribution }% at least one, from doc/KEYWORDS

