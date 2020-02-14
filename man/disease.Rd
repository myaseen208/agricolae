\name{disease}
\alias{disease}
\docType{data}
\title{ Data evaluation of the disease overtime}
\description{
  Three evaluations over time and the potato yield when applying several 
  treatments.
}
\usage{data(disease)}
\format{
  A data frame with 21 observations on the following 7 variables.
  \describe{
    \item{\code{plots}}{a numeric vector}
    \item{\code{rep}}{a numeric vector}
    \item{\code{trt}}{a factor with levels \code{T0} \code{T1} \code{T2} \code{T3} \code{T4} \code{T5} \code{T6}}
    \item{\code{E2}}{a numeric vector}
    \item{\code{E5}}{a numeric vector}
    \item{\code{E7}}{a numeric vector}
    \item{\code{yield}}{a numeric vector}
  }
}

\source{
  Experimental data.
}
\references{
  International Potato Center. CIP - Lima Peru.
}
\examples{
library(agricolae)
data(disease)
str(disease)
}
\keyword{datasets}
