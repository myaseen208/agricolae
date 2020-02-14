\name{duncan.test}
\alias{duncan.test}
%- duncan.
\title{ Duncan's new multiple range test }
\description{
  This test is adapted from the Newman-Keuls method. Duncan's test does not control family wise error rate at the specified alpha level. It has more power than the other post tests, but only because it doesn't control the error rate properly. The Experimentwise Error Rate at: 1-(1-alpha)^(a-1); where "a" is the number of means and is the Per-Comparison Error Rate. Duncan's procedure is only very slightly more conservative than LSD. The level by alpha default is 0.05.
}
\usage{
duncan.test(y, trt, DFerror, MSerror, alpha = 0.05, group=TRUE, main = NULL,console=FALSE)
}

\arguments{
  \item{y}{ model(aov or lm) or answer of the experimental unit  }
  \item{trt}{ Constant( only y=model) or vector treatment applied to each experimental unit  }
  \item{DFerror}{ Degree free }
  \item{MSerror}{ Mean Square Error }
  \item{alpha}{ Significant level }
  \item{group}{ TRUE or FALSE }
  \item{main}{ Title }
  \item{console}{logical, print output }
}
\details{
  It is necessary first makes a analysis of variance.\cr
  
  if y = model, then to apply the instruction:\cr 
  duncan.test(model, "trt", alpha = 0.05, group = TRUE, main = NULL, console = FALSE)\cr
  where the model class is aov or lm.
}
\value{
  \item{statistics}{Statistics of the model}
  \item{parameters}{Design parameters}
  \item{duncan}{Critical Range Table}
  \item{means}{Statistical summary of the study variable}
  \item{comparison}{Comparison between treatments}
  \item{groups}{Formation of treatment groups}
}
\references{  
1. Principles and procedures of statistics a biometrical approach
   Steel & Torry & Dickey. Third Edition 1997\cr
2. Multiple comparisons theory and methods. Departament of statistics 
   the Ohio State University. USA, 1996. Jason C. Hsu. Chapman Hall/CRC.\cr
}

\author{ Felipe de Mendiburu }
\seealso{
  \code{\link{BIB.test}}, \code{\link{DAU.test}}, \code{\link{durbin.test}}, 
  \code{\link{friedman}}, \code{\link{HSD.test}}, \code{\link{kruskal}}, 
  \code{\link{LSD.test}}, \code{\link{Median.test}}, \code{\link{PBIB.test}}, 
  \code{\link{REGW.test}}, \code{\link{scheffe.test}}, \code{\link{SNK.test}},
  \code{\link{waerden.test}}, \code{\link{waller.test}}, \code{\link{plot.group}}
}

\examples{
library(agricolae)
data(sweetpotato)
model<-aov(yield~virus,data=sweetpotato)
out <- duncan.test(model,"virus", 
main="Yield of sweetpotato. Dealt with different virus")
plot(out,variation="IQR")
duncan.test(model,"virus",alpha=0.01,console=TRUE)
# version old duncan.test()
df<-df.residual(model)
MSerror<-deviance(model)/df
out <- with(sweetpotato,duncan.test(yield,virus,df,MSerror, group=TRUE))
plot(out,horiz=TRUE,las=1)
print(out$groups)
}
\keyword{ htest }% at least one, from doc/KEYWORDS

