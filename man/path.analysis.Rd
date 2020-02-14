\name{path.analysis}
\alias{path.analysis}
%- path.analysis.
\title{ Path Analysis }
\description{
  If the cause and effect relationship is well defined, it is possible
  to represent the whole system of variables in a diagram form known as 
  path-analysis. The function calculates the direct and indirect effects
  and uses the variables correlation or covariance.
     }
\usage{
path.analysis(corr.x, corr.y)
}

\arguments{
  \item{corr.x}{ Matrix of correlations of the independent variables }
  \item{corr.y}{ vector of dependent correlations with each one of
  the independent ones }
}
\details{
  It is necessary first to calculate the correlations.
}
\value{
Direct and indirect effects and residual Effect^2.
}
\references{ Biometrical Methods in Quantitative Genetic Analysis, Singh, Chaudhary. 1979 }
\author{ Felipe de Mendiburu }

\seealso{\code{\link{correlation}} }

\examples{
# Path analysis. Multivarial Analysis. Anderson. Prentice Hall, pag 616
library(agricolae)
# Example 1
corr.x<- matrix(c(1,0.5,0.5,1),c(2,2))
corr.y<- rbind(0.6,0.7)
names<-c("X1","X2")
dimnames(corr.x)<-list(names,names)
dimnames(corr.y)<-list(names,"Y")
path.analysis(corr.x,corr.y)
# Example 2
# data of the progress of the disease related bacterial wilt to the ground
# for the component CE Ca K2 Cu
data(wilt)
data(soil)
x<-soil[,c(3,12,14,20)]
y<-wilt[,14]
cor.y<-correlation(y,x)$correlation
cor.x<-correlation(x)$correlation
path.analysis(cor.x,cor.y)

}
\keyword{ multivariate }% at least one, from doc/KEYWORDS

