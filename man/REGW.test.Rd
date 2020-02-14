\name{REGW.test}
\alias{REGW.test}
%- REGW.
\title{ Ryan, Einot and Gabriel and Welsch multiple range test }
\description{
  Multiple range tests for all pairwise comparisons, to obtain a confident inequalities multiple range tests.
}   
\usage{
REGW.test(y, trt, DFerror, MSerror, alpha = 0.05, group=TRUE, main = NULL,console=FALSE)
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
  REGW.test (model, "trt", alpha = 0.05, group = TRUE, main = NULL, console = FALSE)\cr
  where the model class is aov or lm.
}
\value{
  \item{statistics}{Statistics of the model}
  \item{parameters}{Design parameters}
  \item{regw}{Critical Range Table}
  \item{means}{Statistical summary of the study variable}
  \item{comparison}{Comparison between treatments}
  \item{groups}{Formation of treatment groups}
}
\references{ Multiple comparisons theory and methods. Departament of statistics 
 the Ohio State University. USA, 1996. Jason C. Hsu. Chapman Hall/CRC }
\author{ Felipe de Mendiburu }
\seealso{
  \code{\link{BIB.test}}, \code{\link{DAU.test}}, \code{\link{duncan.test}},
  \code{\link{durbin.test}}, \code{\link{friedman}}, \code{\link{HSD.test}},
  \code{\link{kruskal}}, \code{\link{LSD.test}}, \code{\link{Median.test}}, 
  \code{\link{PBIB.test}}, \code{\link{scheffe.test}}, \code{\link{SNK.test}},
  \code{\link{waerden.test}}, \code{\link{waller.test}}, \code{\link{plot.group}}
}
\examples{
library(agricolae)
data(sweetpotato)
model<-aov(yield~virus,data=sweetpotato)
out<- REGW.test(model,"virus", 
main="Yield of sweetpotato. Dealt with different virus")
print(out)
REGW.test(model,"virus",alpha=0.05,console=TRUE,group=FALSE)
}
\keyword{ htest }% at least one, from doc/KEYWORDS

