\name{grass}
\alias{grass}
\docType{data}
\title{ Data for Friedman test}
\description{
  Twelve homeowners are selected randomly to participate in an experiment with
  a plant nursery. Each homeowner is asked to select four fairly identical
  areas in his yard and to plant four different types of grasses, one in each area.
}
\usage{data(grass)}
\format{
  A data frame with 48 observations on the following 3 variables.
  \describe{
    \item{\code{judge}}{a numeric vector}
    \item{\code{trt}}{a factor with levels \code{t1} \code{t2} \code{t3} \code{t4}}
    \item{\code{evaluation}}{a numeric vector}
  }
}
\details{
  Each of the 12 blocks consists of four fairly identical plots of land,
  each receiving care of approximately the same degree of skill because the
  four plots are presumably cared for by the same homeowern.
}
\source{
  Book: Practical Nonparametrics Statistics, pag 372.
}
\references{
  Practical Nonparametrics Statistics. W.J. Conover, 1999
}
\examples{
data(grass)
str(grass)
}
\keyword{datasets}
