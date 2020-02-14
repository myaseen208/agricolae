\name{AMMI.contour}
\alias{AMMI.contour}
%- AMMI.contour.
\title{ AMMI contour }
\description{
  Draws a polygon or a circumference around the center of the Biplot with a 
  proportional radio at the longest distance of the genotype.
}
\usage{
AMMI.contour(model, distance, shape, ...)
}
\arguments{
  \item{model}{ Object }
  \item{distance}{ Circumference radius >0 and <=1 }
  \item{shape}{ Numerical, relating to the shape of the polygon outline. }
  \item{\dots}{ Parameters corresponding to the R lines function }
}
\details{
First, it is necessary to execute the AMMI function.
It is only valid for the BIPLOT function but not for the TRIPLOT one.
}
\value{
Genotypes within and outside the area.

\item{distance}{Distance from genotype to origin (0,0) }
}
\author{ Felipe de Mendiburu }
\note{ 
 Complement graphics AMMI 
}
\seealso{\code{\link{AMMI}} }
\examples{
library(agricolae)
# see AMMI.
data(sinRepAmmi)
Environment <- sinRepAmmi$ENV
Genotype <- sinRepAmmi$GEN
Yield <- sinRepAmmi$YLD
REP <- 3
MSerror <- 93.24224
model<-AMMI(Environment, Genotype, REP, Yield, MSerror)
plot(model)
AMMI.contour(model,distance=0.7,shape=8,col="red",lwd=2,lty=5)
}
\keyword{ aplot }% __ONLY ONE__ keyword per line
