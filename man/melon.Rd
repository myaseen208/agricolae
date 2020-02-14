\name{melon}
\alias{melon}
\docType{data}
\title{ Data of yield of melon in a Latin square experiment }
\description{
  An irrigation system evaluation by exudation using four varieties of 
  melon, under modality of sowing, SIMPLE ROW.
  The goal is to analyze the behavior of three hybrid melon varieties 
  and one standard.
}
\usage{data(melon)}
\format{
  A data frame with 16 observations on the following 4 variables.
  \describe{
    \item{\code{row}}{a numeric vector}
    \item{\code{col}}{a numeric vector}
    \item{\code{variety}}{a factor with levels \code{V1} \code{V2} \code{V3} \code{V4}}
    \item{\code{yield}}{a numeric vector}
  }
}
\details{Varieties:	
Hibrido Mission (V1); Hibrido Mark (V2); Hibrido Topfligth (V3); Hibrido Hales Best Jumbo (V4).
}
\source{
  Tesis. "Evaluacion del sistema de riego por exudacion utilizando cuatro variedades
  de melon, bajo modalidad de siembra, SIMPLE HILERA". Alberto Angeles L. 
  Universidad Agraria la Molina - Lima Peru.
}
\references{
 Universidad Nacional Agraria la molina.
}
\examples{
library(agricolae)
data(melon)
str(melon)
}
\keyword{datasets}

