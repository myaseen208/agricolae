\name{nonadditivity}
\alias{nonadditivity}
%- nonadditivity.
\title{ Nonadditivity model test }
\description{
  The resistance for the transformable nonadditivity, due to J. W. Tukey, 
  is based on the detection of a curvilinear relation between y-est(y)
  and est(y). A freedom degree for the transformable nonadditivity.
}
\usage{
nonadditivity(y, factor1, factor2, df, MSerror)
}

\arguments{
  \item{y}{ Answer of the experimental unit  }
  \item{factor1}{ Firts treatment applied to each experimental unit }
  \item{factor2}{ Second treatment applied to each experimental unit }
  \item{df}{ Degrees of freedom of the experimental error }
  \item{MSerror}{ Means square error of the experimental }
}
\details{
Only two factor: Block and treatment or factor 1 and factor 2.
}
\value{
P, Q and non-additivity analysis of variance 
}
\references{ 
1. Steel, R.; Torri,J; Dickey, D.(1997)
   Principles and Procedures of Statistics
   A Biometrical Approach

2. George E.P. Box; J. Stuart Hunter and William G. Hunter. 
   Statistics for experimenters. 
   Wile Series in probability and statistics }
\author{ Felipe de Mendiburu }

\examples{
library(agricolae)
data(potato )
potato[,1]<-as.factor(potato[,1])
model<-lm(cutting ~ date + variety,potato)
df<-df.residual(model)
MSerror<-deviance(model)/df
analysis<-with(potato,nonadditivity(cutting, date, variety, df, MSerror))
}
\keyword{ models  }% at least one, from doc/KEYWORDS

