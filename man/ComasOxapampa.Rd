\name{ComasOxapampa}
\alias{ComasOxapampa}
\docType{data}
\title{ Data AUDPC Comas - Oxapampa}
\description{
  Fifty-three potato varieties developed by the breeding program of the 
  International Potato Center and released in different countries around 
  the world were evaluated for their resistance to late blight in two locations 
  in Peru.
}  
\usage{data(ComasOxapampa)}
\format{
  A data frame with 168 observations on the following 4 variables.
  \describe{
    \item{\code{cultivar}}{a factor with 56 levels }
    \item{\code{replication}}{a factor with 3 levels }
    \item{\code{comas}}{a numeric vector}
    \item{\code{oxapampa}}{a numeric vector}
  }
}

\details{
  The experimental design was a randomized complete block design with 3 replications 
  of 15 apical stem cuttings in Oxapampa and 10 tubers in Mariscal Castilla.
  Plots were 11.9 x 18.5 m in size with 30 cm in-row and 0.9 m between-row spacings.
  Spreader rows around plots were used at each site. Mancozeb was applied weekly
  until 30 days after transplanting or planting, after which the plants were left
  to natural infection.
  Due to climatic conditions not conductive to the disease
  in Oxapampa, inoculum was enhanced with local isolate (POX 067, with virulence
  R1, 2, 3, 4, 5, 6, 7, 10, 11) at a concentration of 5000-sporangia/ ml at 49 days
  after planting. Percentage of foliar infection was estimated visually every 3 days
  for 8 times in Oxapampa and every 7 days for 12 times in Comas, then values were
  converted to the relative area under the diseases progress curve (rAUPDC).
  rAUDPC rankings were analyzed for phenotypic stability with nonparametric measures.
  }
\source{
  Experimental field, 2002. Data Kindly provided by Wilmer Perez.
}
\references{
  International Potato Center. CIP - Lima Peru.
}
\examples{
library(agricolae)
data(ComasOxapampa)
# Oxapampa (10 35 31 S latitude, 75 23 0 E longitude, 1813 m.a.s.l )
# Comas, Mariscal Castilla (11  42  54  S latitude, 75 04 45 E longitude, 2800 m.a.s.l,)
# cultivars LBr-40 (resistant), Cruza 148 (moderately resistant) and Pimpernell (susceptible)
str(ComasOxapampa)
means <- tapply.stat(ComasOxapampa[,3:4],ComasOxapampa$cultivar,mean)
correlation(means$comas,means$oxapampa, method="kendall")
}
\keyword{datasets}
