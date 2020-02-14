\name{table.freq}
\alias{table.freq}
%- table.freq.
\title{ frequency Table of a Histogram  }
\description{
  It finds the absolute, relative and accumulated frequencies with the class 
  intervals defined from a previously calculated histogram by the "hist" of R
  function.
}
\usage{
table.freq(object)
}

\arguments{
  \item{object}{ Object by function graph.freq() }
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
print(table.freq(h2),row.names=FALSE)
}
\keyword{ distribution }% at least one, from doc/KEYWORDS

