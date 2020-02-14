\name{plrv}
\alias{plrv}
\docType{data}
\title{ Data clones from the PLRV population }
\description{
  Six environments: Ayacucho, La Molina 02, San Ramon 02, Huancayo,
  La Molina 03, San Ramon 03.
}
\usage{data(plrv)}
\format{
  A data frame with 504 observations on the following 6 variables.
  \describe{
    \item{\code{Genotype}}{a factor with levels \code{102.18} \code{104.22} \code{121.31} \code{141.28} \code{157.26} \code{163.9} \code{221.19} \code{233.11} \code{235.6} \code{241.2} \code{255.7} \code{314.12} \code{317.6} \code{319.20} \code{320.16} \code{342.15} \code{346.2} \code{351.26} \code{364.21} \code{402.7} \code{405.2} \code{406.12} \code{427.7} \code{450.3} \code{506.2} \code{Canchan} \code{Desiree} \code{Unica}}
    \item{\code{Locality}}{a factor with levels \code{Ayac} \code{Hyo-02} \code{LM-02} \code{LM-03} \code{SR-02} \code{SR-03}}
    \item{\code{Rep}}{a numeric vector}
    \item{\code{WeightPlant}}{a numeric vector}
    \item{\code{WeightPlot}}{a numeric vector}
    \item{\code{Yield}}{a numeric vector}
  }
}

\source{
  International Potato Center Lima-Peru
  }
\references{
  International Potato Center Lima-Peru
}
\examples{
library(agricolae)
data(plrv)
str(plrv)
}
\keyword{datasets}
