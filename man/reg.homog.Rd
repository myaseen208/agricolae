\name{reg.homog}
\alias{reg.homog}
%- reg.homog.
\title{ Homologation of regressions }
\description{
  It makes the regressions homogeneity test for a group of treatments where
  each observation presents a linearly dependent reply from another one. 
  There is a linear function in every treatment.
  The objective is to find out if the linear models of each treatment
  come from the same population.
}
\usage{
reg.homog(trt, x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{trt}{ treatment }
  \item{x}{ independent variable }
  \item{y}{ dependent variable }
}
\value{
list objects:\cr
Number regressions.\cr
Residual.\cr
Difference of regression.\cr
DF.homgeneity (homogenity degree free).\cr
DF.Residual (degree free error).\cr
F.value. Test statitics. \cr
P.value. P Value (Significant\cr
Criterion. conclusion\cr

}
\references{ Book in Spanish: Metodos estadisticos para la investigacion.
             Calzada Benza 1960 }
\author{ Felipe de Mendiburu }

\examples{
library(agricolae)
data(frijol)
evaluation<-with(frijol,reg.homog(technology,index,production))
# Example 2. Applied Regression Analysis a Research tools
# 1988. John O.Rawlings. Wadsworth & brooks/cole Advanced Books
# & Software. Pacific Grove. Califonia.
# Statistics/probability. Series
LineNumber<-c(rep("39","30"),rep("52","30"))
PlantingDate<-rep(c("16","20","21"),20)
HeadWt <- c(2.5,3.0,2.2,2.2,2.8,1.8,3.1,2.8,1.6,4.3,2.7,2.1,2.5,2.6,3.3,4.3,
2.8,3.8,3.8,2.6,3.2,4.3,2.6,3.6,1.7,2.6,4.2,3.1,3.5,1.6,2.0,4.0,1.5,2.4,2.8,
1.4,1.9,3.1,1.7,2.8,4.2,1.3,1.7,3.7,1.7,3.2,3.0,1.6,2.0,2.2,1.4,2.2,2.3,1.0,
2.2,3.8,1.5,2.2,2.0,1.6)
Ascorbic <-c(51,65,54,55,52,59,45,41,66,42,51,54,53,41,45,50,45,49,50,51,49,
52,45,55,56,61,49,49,42,68,58,52,78,55,70,75,67,57,70,61,58,84,67,47,71,68,
56,72,58,72,62,63,63,68,56,54,66,72,60,72)
trt<-paste(LineNumber,PlantingDate,sep="-")
output<-reg.homog(trt,HeadWt,Ascorbic)
}
\keyword{ regression }% at least one, from doc/KEYWORDS

