\name{resampling.cv}
\alias{resampling.cv}
%- resampling.cv.
\title{ Resampling to find the optimal number of markers }
\description{
  This process finds the curve of CV for a different number of markers which
  allows us to determine the number of optimal markers for a given relative 
  variability.
  A method of the curvature.
}
\usage{
resampling.cv(A, size, npoints)
}

\arguments{
  \item{A}{ data frame or matrix of binary data }
  \item{size}{ number of re-samplings }
  \item{npoints}{ Number of points to consider the model }
}
\value{
lm(formula = CV ~ I(1/marker))\cr
Table with variation coefficient by number of markers
}

\references{Efron, B., Tibshirani, R. (1993) An Introduction to the Boostrap. Chapman and Hall/CRC }
\author{ Felipe de Mendiburu }

\seealso{\code{\link{cv.similarity}}, \code{\link{similarity} } }

\examples{
library(agricolae)
#example table of molecular markers
data(markers)
study<-resampling.cv(markers,size=1,npoints=15)
#
# Results of the model
summary(study$model)
coef<-coef(study$model)
py<-predict(study$model)
Rsq<-summary(study$model)$"r.squared"
table.cv <- data.frame(study$table.cv,estimate=py)
print(table.cv)

# Plot CV
#startgraph
limy<-max(table.cv[,2])+10
plot(table.cv[,c(1,2)],col="red",frame=FALSE,xlab="number of markers",
ylim=c(10,limy), ylab="CV",cex.main=0.8,main="Estimation of the number of markers")
ty<-quantile(table.cv[,2],1)
tx<-median(table.cv[,1])
tz<-quantile(table.cv[,2],0.95)
text(tx,ty, cex=0.8,as.expression(substitute(CV == a + frac(b,markers),
list(a=round(coef[1],2),b=round(coef[2],2)))) )
text(tx,tz,cex=0.8,as.expression(substitute(R^2==r,list(r=round(Rsq,3)))))

# Plot CV = a + b/n.markers
fy<-function(x,a,b) a+b/x
x<-seq(2,max(table.cv[,1]),length=50)
y <- coef[1] + coef[2]/x
lines(x,y,col="blue")
#grid(col="brown")
rug(table.cv[,1])
#endgraph
}
\keyword{ optimize }% at least one, from doc/KEYWORDS

