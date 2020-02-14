\name{ralstonia}
\alias{ralstonia}
\docType{data}
\title{ Data of assessment of the population in the soil R.solanacearum}
\description{
  The assessment of the population of R.solanacearum on the floor took place 
  after 48 hours of infestation, during days 15, 29, 43, 58, and 133 days 
  after the infestation soil. More information on soil data(soil).
}
\usage{data(ralstonia)}
\format{
  A data frame with 13 observations on the following 8 variables.
  \describe{
    \item{\code{place}}{a factor with levels \code{Chmar} \code{Chz} \code{Cnt1} \code{Cnt2} \code{Cnt3} \code{Hco1} \code{Hco2} \code{Hco3} \code{Hyo1} \code{Hyo2} \code{Namora} \code{SR1} \code{SR2}}
    \item{\code{Day2}}{a numeric vector}
    \item{\code{Day15}}{a numeric vector}
    \item{\code{Day29}}{a numeric vector}
    \item{\code{Day43}}{a numeric vector}
    \item{\code{Day58}}{a numeric vector}
    \item{\code{Day73}}{a numeric vector}
    \item{\code{Day133}}{a numeric vector}
  }
}
\details{
  Logarithm average counts of colonies on plates containing half of M-SMSA 
  3 repetitions (3 plates by repetition) incubated at 30 degrees centigrade 
  for 48 hours. log(1+UFC/g soil) }
\source{
  Experimental field, 2004. Data Kindly provided by Dr. Sylvie Priou, Liliam Gutarra and Pedro Aley.
}

\references{
  International Potato Center. CIP - Lima Peru.
}
\examples{
library(agricolae)
data(ralstonia)
str(ralstonia)
}
\keyword{datasets}
