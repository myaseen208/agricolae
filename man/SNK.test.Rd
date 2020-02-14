\name{SNK.test}
\alias{SNK.test}
%- SNK.
\title{ Student-Newman-Keuls (SNK) }
\description{
  SNK is derived from Tukey, but it is less conservative (finds more differences).
  Tukey controls the error for all comparisons, where SNK only controls for 
  comparisons under consideration. The level by alpha default is 0.05.
}
\usage{
SNK.test(y, trt, DFerror, MSerror, alpha = 0.05, group=TRUE, main = NULL,console=FALSE)
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
  SNK.test (model, "trt", alpha = 0.05, group = TRUE, main = NULL, console = FALSE)\cr
  where the model class is aov or lm.
  
}
\value{
  \item{statistics}{Statistics of the model}
  \item{parameters}{Design parameters}
  \item{snk}{Critical Range Table}
  \item{means}{Statistical summary of the study variable}
  \item{comparison}{Comparison between treatments}
  \item{groups}{Formation of treatment groups}
}
\references{  
1. Principles and procedures of statistics a biometrical approach
   Steel & Torry & Dickey. Third Edition 1997\cr
2. Multiple comparisons theory and methods. Departament of statistics 
   the Ohio State University. USA, 1996. Jason C. Hsu. Chapman Hall/CRC.
}

\author{ Felipe de Mendiburu }
\seealso{
  \code{\link{BIB.test}}, \code{\link{DAU.test}}, \code{\link{duncan.test}},
  \code{\link{durbin.test}}, \code{\link{friedman}}, \code{\link{HSD.test}},
  \code{\link{kruskal}}, \code{\link{LSD.test}}, \code{\link{Median.test}}, 
  \code{\link{PBIB.test}}, \code{\link{REGW.test}}, \code{\link{scheffe.test}},
  \code{\link{waerden.test}}, \code{\link{waller.test}}, \code{\link{plot.group}}
}
\examples{
library(agricolae)
data(sweetpotato)
model<-aov(yield~virus,data=sweetpotato)
out <- SNK.test(model,"virus", console=TRUE, 
main="Yield of sweetpotato. Dealt with different virus")
print(SNK.test(model,"virus", group=FALSE))
# version old SNK.test()
df<-df.residual(model)
MSerror<-deviance(model)/df
out <- with(sweetpotato,SNK.test(yield,virus,df,MSerror, group=TRUE))
print(out$groups)
}
\keyword{ htest }% at least one, from doc/KEYWORDS

