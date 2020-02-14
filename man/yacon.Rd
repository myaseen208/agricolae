\name{yacon}
\alias{yacon}
\docType{data}
\title{ Data Yacon}
\description{
The yacon (Smallanthus sonchifolius) is a plant native to the Andes, considered a traditional crop in Peru and
natural source of FOS, which is a type of carbohydrate that can not be digested by the
and the human body that have joined several beneficial properties in health, such as
improve the absorption of calcium, reducing the level of triglycerides and cholesterol
and stimulate better gastrointestinal function.
}
\usage{data(yacon)}
\format{
  A data frame with 432 observations on the following 19 variables.
  \describe{
    \item{\code{locality}}{a factor with levels, Cajamarca, Lima, Oxapampa in PERU}
    \item{\code{site}}{a numeric vector}
    \item{\code{dose}}{a factor with levels \code{F0} \code{F150} \code{F80}}
    \item{\code{entry}}{a factor with levels \code{AKW5075 } \code{AMM5136} \code{AMM5150} \code{AMM5163} \code{ARB5125 } \code{CLLUNC118} \code{P1385} \code{SAL136}}
    \item{\code{replication}}{a numeric vector, replications}
    \item{\code{height}}{a numeric vector, plant height, centimeters}
    \item{\code{stalks}}{a numeric vector, number of stalks}
    \item{\code{wfr}}{a numeric vector, weight of fresh roots, grams}
    \item{\code{wff}}{a numeric vector, weight of fresh foliage, grams}
    \item{\code{wfk}}{a numeric vector, weight fresh kroner, grams}
    \item{\code{roots}}{a numeric vector, matter of dried roots, grams}
    \item{\code{FOS}}{a numeric vector, fructo-oligosaccharides, percentaje}
    \item{\code{glucose}}{a numeric vector, percentaje}
    \item{\code{fructose}}{a numeric vector, percentaje}
    \item{\code{sucrose}}{a numeric vector, percentaje}
    \item{\code{brix}}{a numeric vector, degrees Brix}
    \item{\code{foliage}}{a numeric vector, matter dry foliage, grams}
    \item{\code{dry}}{a numeric vector, dry matter kroner, grams}
    \item{\code{IH}}{a numeric vector, Index harvest, 0 to 1}
  }
}
\details{
Proportion or fraction of the plant that is used (seeds, fruit, root) on dry basis.
Part usable in a proportion of total mass dissected. Plant of frijol, weight = 100g
and frijol = 50g then, IH = 50/100 = 0.5 or 50 percentaje. Degrees Brix is 
a measurement of the mass ratio of dissolved sugar to water in a liquid.
}
\source{
CIP. Experimental field, 2003, Data Kindly provided by Ivan Manrique and Carolina Tasso.
}
\references{
 International Potato Center. CIP - Lima Peru.
}
\examples{
library(agricolae)
data(yacon)
str(yacon) 
}
\keyword{datasets}
