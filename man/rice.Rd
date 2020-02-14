\name{rice}
\alias{rice}
\docType{data}
\title{ Data of Grain yield of rice variety IR8}
\description{
  The data correspond to the yield of rice variety IR8 (g/m2) for land uniformity 
  studies. The growing area is 18x36 meters.
}
\usage{data(rice)}
\format{
  A data frame with 36 observations on the following 18 variables.
  \describe{
    \item{\code{V1}}{a numeric vector}
    \item{\code{V2}}{a numeric vector}
    \item{\code{V3}}{a numeric vector}
    \item{\code{V4}}{a numeric vector}
    \item{\code{V5}}{a numeric vector}
    \item{\code{V6}}{a numeric vector}
    \item{\code{V7}}{a numeric vector}
    \item{\code{V8}}{a numeric vector}
    \item{\code{V9}}{a numeric vector}
    \item{\code{V10}}{a numeric vector}
    \item{\code{V11}}{a numeric vector}
    \item{\code{V12}}{a numeric vector}
    \item{\code{V13}}{a numeric vector}
    \item{\code{V14}}{a numeric vector}
    \item{\code{V15}}{a numeric vector}
    \item{\code{V16}}{a numeric vector}
    \item{\code{V17}}{a numeric vector}
    \item{\code{V18}}{a numeric vector}
  }
}
\details{
  Table 12.1 Measuring Soil Heterogeneity
}
\source{
  Statistical Procedures for Agriculture Research. Second Edition.
  Kwanchai A. Gomez and Arturo A. Gomez. 1976. USA
  Pag. 481.
}
\references{
  Statistical Procedures for Agriculture Research. Second Edition.
  Kwanchai A. Gomez and Arturo A. Gomez. 1976. USA
}
\examples{
library(agricolae)
data(rice)
str(rice)
}
\keyword{datasets}
