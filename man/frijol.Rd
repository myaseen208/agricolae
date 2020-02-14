\name{frijol}
\alias{frijol}
\docType{data}
\title{ Data of frijol }
\description{
  Data of frijol under 4 technologies for the homogeneity of regression study.
  Yield of Frijol in kg/ha in clean and dry grain.

  Tecnnologies:
  20-40-20 kg/ha.  N. P2O5 and K2O + 2 t/ha of gallinaza.
  40-80-40 kg/ha.  N. P2O5 and K2O + 2 t/ha of gallinaza.
  60-120-60 kg/ha. N. P2O5 and K2O + 2 t/ha of gallinaza.
  40-80-40 kg/ha.  N. P2O5 and K2O + 4 t/ha of gallinaza.
}
\usage{data(frijol)}
\format{
  A data frame with 84 observations on the following 3 variables.
  \describe{
    \item{\code{technology}}{a factor with levels \code{a} \code{b} \code{c} \code{d}}
    \item{\code{production}}{a numeric vector}
    \item{\code{index}}{a numeric vector}
  }
}

\references{
Oriente antioqueno (1972) (ICA.- Orlando Martinez W.)
Colombia.
}
\examples{
library(agricolae)
data(frijol)
str(frijol)
}
\keyword{datasets}

