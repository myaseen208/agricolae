\name{genxenv}
\alias{genxenv}
\docType{data}
\title{ Data of potato yield in a different environment }
\description{
  50 genotypes and 5 environments.
}
\usage{data(genxenv)}
\format{
  A data frame with 250 observations on the following 3 variables.
  \describe{
    \item{\code{ENV}}{a numeric vector}
    \item{\code{GEN}}{a numeric vector}
    \item{\code{YLD}}{a numeric vector}
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
data(genxenv)
str(genxenv)
}
\keyword{datasets}
