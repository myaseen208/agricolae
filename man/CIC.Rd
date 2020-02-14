\name{CIC}
\alias{CIC}
\docType{data}
\title{ Data for late blight of potatoes }
\description{
A study of Phytophthora infestans in the potato plant in the localities of Comas and
Oxapampa in Peru, 2005.
}
\usage{data(CIC)}
\format{
  The format is:
List of 2 (comas, oxapampa)
  \describe{
    \item{\code{Locality}}{a factor with levels \code{Comas} \code{Oxapampa} }
    \item{\code{Genotype}}{a factor}
    \item{\code{Rep}}{a numeric vector, replications}
    \item{\code{E9}}{a numeric vector, infestans percentaje to 9 days}
    \item{\code{AUDPC}}{a numeric vector: the area under the disease-progress curve}  
    \item{\code{Relative}}{a numeric vector, relative area}  
  }
}

\details{
  comas: temperature=59.9 Fahrenheit, relative humidity=83.3
  oxapampa: temperature=64.8 Fahrenheit, relative humidity=86.2
  AUDPC and relative see function audpc(). help(audpc)
  Exx: Evaluation in percentaje, xx is days. 
  ORD1, ORD2, SBLK and row are references location of the plot in the field.
  }
\source{
  Experimental field, 2004-2005. Data Kindly provided by Matilde Orrillo.
}
\references{
  International Potato Center. CIP - Lima Peru. 
}
\examples{
library(agricolae)
data(CIC)
CIC$comas
CIC$oxapampa
}
\keyword{datasets}
