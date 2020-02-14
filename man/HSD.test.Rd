\name{HSD.test}
\alias{HSD.test}
%- HSD.
\title{ Multiple comparisons: Tukey  }
\description{
  It makes multiple comparisons of treatments by means of Tukey.
  The level by alpha default is 0.05.
}
\usage{
HSD.test(y, trt, DFerror, MSerror, alpha = 0.05, group=TRUE, main = NULL, 
unbalanced=FALSE,console=FALSE)
}

\arguments{
  \item{y}{ model(aov or lm) or answer of the experimental unit  }
  \item{trt}{ Constant( only y=model) or vector treatment applied to each experimental unit  }
  \item{DFerror}{ Degree free }
  \item{MSerror}{ Mean Square Error }
  \item{alpha}{ Significant level }
  \item{group}{ TRUE or FALSE }
  \item{main}{ Title }
  \item{unbalanced}{ TRUE or FALSE. not equal replication}  
  \item{console}{logical, print output }
}
\details{
  It is necessary first makes a analysis of variance.\cr
  
  if y = model, then to apply the instruction:\cr 
  HSD.test (model, "trt", alpha = 0.05, group = TRUE, main = NULL, unbalanced=FALSE, console=FALSE)\cr
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
1. Principles and procedures of statistics a biometrical approach
   Steel & Torry & Dickey. Third Edition 1997\cr
2. Multiple comparisons theory and methods. Departament of statistics 
   the Ohio State University. USA, 1996. Jason C. Hsu. Chapman Hall/CRC.
}

\author{ Felipe de Mendiburu }
\seealso{
  \code{\link{BIB.test}}, \code{\link{DAU.test}}, \code{\link{duncan.test}},
  \code{\link{durbin.test}}, \code{\link{friedman}}, \code{\link{kruskal}}, 
  \code{\link{LSD.test}}, \code{\link{Median.test}}, \code{\link{PBIB.test}}, 
  \code{\link{REGW.test}}, \code{\link{scheffe.test}}, \code{\link{SNK.test}},
  \code{\link{waerden.test}}, \code{\link{waller.test}}, \code{\link{plot.group}}
}
\examples{
library(agricolae)
data(sweetpotato)
model<-aov(yield~virus, data=sweetpotato)
out <- HSD.test(model,"virus", group=TRUE,console=TRUE,
main="Yield of sweetpotato\nDealt with different virus")
#stargraph
# Variation range: max and min
plot(out)
#endgraph
out<-HSD.test(model,"virus", group=FALSE)
print(out$comparison)
# Old version HSD.test()
df<-df.residual(model)
MSerror<-deviance(model)/df
with(sweetpotato,HSD.test(yield,virus,df,MSerror, group=TRUE,console=TRUE,
main="Yield of sweetpotato. Dealt with different virus"))
}
\keyword{ htest }% at least one, from doc/KEYWORDS

