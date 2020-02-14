\name{clay}
\alias{clay}
\docType{data}
\title{ Data of Ralstonia population in clay soil}
\description{
  An evaluation over a time period.
}
\usage{data(clay)}
\format{
  A data frame with 69 observations on the following 3 variables.
  \describe{
    \item{\code{per.clay}}{a numeric vector}
    \item{\code{days}}{a numeric vector}
    \item{\code{ralstonia}}{a numeric vector}
  }
}

\source{
  Experimental field.
}
\references{
  International Potato Center. CIP - Lima Peru.
}
\examples{
library(agricolae)
data(clay)
str(clay)
}
\keyword{datasets}
