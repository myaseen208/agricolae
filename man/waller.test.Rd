\name{waller.test}
\alias{waller.test}
%- waller.test.
\title{ Multiple comparisons, Waller-Duncan }
\description{
  The Waller-Duncan k-ratio t test is performed on all main effect means
  in the MEANS statement. See the K-RATIO option for information on 
  controlling details of the test.
}
\usage{
waller.test(y, trt, DFerror, MSerror, Fc, K = 100, group=TRUE, main = NULL, 
console=FALSE)
}
\arguments{
  \item{y}{ model(aov or lm) or answer of the experimental unit  }
  \item{trt}{ Constant( only y=model) or vector treatment applied to each unit}
  \item{DFerror}{ Degrees of freedom }
  \item{MSerror}{ Mean Square Error }
  \item{Fc}{ F Value }
  \item{K}{ K-RATIO }
  \item{group}{ TRUE or FALSE }
  \item{main}{ Title }
  \item{console}{logical, print output }
}
\details{

It is necessary first makes a analysis of variance.\cr

K-RATIO (K): value specifies the Type 1/Type 2 error seriousness ratio for 
the Waller-Duncan test. Reasonable values for KRATIO are 50, 100, and 500,
which roughly correspond for the two-level case to ALPHA levels of 0.1, 0.05,
and 0.01. By default, the procedure uses the default value of 100.\cr

 if y = model, then to apply the instruction:\cr 
 waller.test (model, "trt", alpha = 0.05, group = TRUE, main = NULL, console = FALSE)\cr
 where the model class is aov or lm.
}
\value{
  \item{statistics}{Statistics of the model}
  \item{parameters}{Design parameters}
  \item{means}{Statistical summary of the study variable}
  \item{comparison}{Comparison between treatments}
  \item{groups}{Formation of treatment groups}
}
\references{ 
Waller, R. A. and Duncan, D. B. (1969).
A Bayes Rule for the Symmetric Multiple Comparison Problem,
Journal of the American Statistical Association 64, pages 1484-1504.\cr

Waller, R. A. and Kemp, K. E. (1976)
Computations of Bayesian t-Values for Multiple Comparisons,
Journal of Statistical Computation and Simulation, 75, pages 169-172.\cr

Steel & Torry & Dickey. Third Edition 1997
Principles and procedures of statistics a biometrical approach
}
\author{ Felipe de Mendiburu}
\seealso{
  \code{\link{BIB.test}}, \code{\link{DAU.test}}, \code{\link{duncan.test}},
  \code{\link{durbin.test}}, \code{\link{friedman}}, \code{\link{HSD.test}},
  \code{\link{kruskal}}, \code{\link{LSD.test}}, \code{\link{Median.test}}, 
  \code{\link{PBIB.test}}, \code{\link{REGW.test}}, \code{\link{scheffe.test}}, 
  \code{\link{SNK.test}}, \code{\link{waerden.test}}, \code{\link{plot.group}}
}
\examples{
library(agricolae)
data(sweetpotato)
model<-aov(yield~virus, data=sweetpotato)
out <- waller.test(model,"virus", group=TRUE)
#startgraph
oldpar<-par(mfrow=c(2,2))
# variation: SE is error standard
# variation: range is Max - Min
bar.err(out$means,variation="SD",horiz=TRUE,xlim=c(0,45),bar=FALSE,
col=colors()[25],space=2, main="Standard deviation",las=1)
bar.err(out$means,variation="SE",horiz=FALSE,ylim=c(0,45),bar=FALSE,
col=colors()[15],space=2,main="SE",las=1)
bar.err(out$means,variation="range",ylim=c(0,45),bar=FALSE,col="green",
space=3,main="Range = Max - Min",las=1)
bar.group(out$groups,horiz=FALSE,ylim=c(0,45),density=8,col="red", 
main="Groups",las=1)
#endgraph
# Old version HSD.test()
df<-df.residual(model)
MSerror<-deviance(model)/df
Fc<-anova(model)["virus",4]
out <- with(sweetpotato,waller.test(yield, virus, df, MSerror, Fc, group=TRUE))
print(out)
par(oldpar)
}
\keyword{ htest }% at least one, from doc/KEYWORDS

