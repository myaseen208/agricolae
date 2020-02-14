\name{growth}
\alias{growth}
\docType{data}
\title{ Data growth of trees}
\description{
  Data growth of pijuayo trees in several localities.
}
\usage{data(growth)}
\format{
  A data frame with 30 observations on the following 3 variables.
  \describe{
    \item{\code{place}}{a factor with levels \code{L1} \code{L2}}
    \item{\code{slime}}{a numeric vector}
    \item{\code{height}}{a numeric vector}
  }
}

\source{
  Experimental data (Pucallpa - Peru)
}
\references{
  ICRAF lima Peru.
}
\examples{
library(agricolae)
data(growth)
str(growth)
}
\keyword{datasets}
