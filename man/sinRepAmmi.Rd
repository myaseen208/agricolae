\name{sinRepAmmi}
\alias{sinRepAmmi}
\docType{data}
\title{ Data for AMMI without repetition}
\description{
  Data frame for AMMI analysis with 50 genotypes in 5 environments.
}
\usage{data(sinRepAmmi)}
\format{
  A data frame with 250 observations on the following 3 variables.
  \describe{
    \item{\code{ENV}}{a factor with levels \code{A1} \code{A2} \code{A3} \code{A4} \code{A5}}
    \item{\code{GEN}}{a numeric vector}
    \item{\code{YLD}}{a numeric vector}
  }
}

\source{
  Experimental data.
}
\references{
  International Potato Center - Lima Peru.
}
\examples{
library(agricolae)
data(sinRepAmmi)
str(sinRepAmmi)
}
\keyword{datasets}
