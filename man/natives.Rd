\name{natives}
\alias{natives}
\docType{data}
\title{ Data of native potato }
\description{
  An evaluation of the number, weight and size of 24 native potatoes varieties.
}
\usage{data(natives)}
\format{
  A data frame with 876 observations on the following 4 variables.
  \describe{
    \item{\code{variety}}{a numeric vector}
    \item{\code{number}}{a numeric vector}
    \item{\code{weight}}{a numeric vector}
    \item{\code{size}}{a numeric vector}
  }
}

\source{
  International Potato Center. CIP - Lima Peru.
}

\examples{
library(agricolae)
data(natives)
str(natives)
}
\keyword{datasets}
