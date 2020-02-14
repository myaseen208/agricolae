\name{Glycoalkaloids}
\alias{Glycoalkaloids}
\docType{data}
\title{ Data Glycoalkaloids}
\description{
  A measurement of the Glycoalkaloids by two methods: HPLC and spectrophotometer.
}
\usage{data(Glycoalkaloids)}
\format{
  A data frame with 25 observations on the following 2 variables.
  \describe{
    \item{\code{HPLC}}{a numeric vector}
    \item{\code{spectrophotometer}}{a numeric vector}
  }
}

\source{
  International Potato Center. CIP - Lima Peru.
}
\references{
  International Potato Center. CIP - Lima Peru.
}
\examples{
library(agricolae)
data(Glycoalkaloids)
str(Glycoalkaloids)
}
\keyword{datasets}
