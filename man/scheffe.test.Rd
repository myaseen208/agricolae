\name{scheffe.test}
\alias{scheffe.test}
%- scheffe.test.
\title{ Multiple comparisons, scheffe }
\description{
  Scheffe 1959, method is very general in that all possible contrasts 
  can be tested for significance and confidence intervals can be 
  constructed for the corresponding linear. The test is conservative. 
  }
\usage{
scheffe.test(y, trt, DFerror, MSerror, Fc, alpha = 0.05, group=TRUE, main = NULL,
console=FALSE )
}
\arguments{
  \item{y}{ model(aov or lm) or answer of the experimental unit  }
  \item{trt}{ Constant( only y=model) or vector treatment applied to each experimental unit  }
  \item{DFerror}{ Degrees of freedom }
  \item{MSerror}{ Mean Square Error }
  \item{Fc}{ F Value }
  \item{alpha}{ Significant level }
  \item{group}{ TRUE or FALSE }
  \item{main}{ Title }
  \item{console}{logical, print output }
}
\details{
  It is necessary first makes a analysis of variance.\cr
  
  if y = model, then to apply the instruction:\cr 
  scheffe.test (model, "trt", alpha = 0.05, group = TRUE, main = NULL, console = FALSE)\cr
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
Robert O. Kuehl. 2nd ed. Design of experiments. Duxbury, copyright 2000.\cr
Steel, R.; Torri,J; Dickey, D.(1997) Principles and Procedures of Statistics
A Biometrical Approach. pp189
}
\author{ Felipe de Mendiburu}
\seealso{
  \code{\link{BIB.test}}, \code{\link{DAU.test}}, \code{\link{duncan.test}},
  \code{\link{durbin.test}}, \code{\link{friedman}}, \code{\link{HSD.test}},
  \code{\link{kruskal}}, \code{\link{LSD.test}}, \code{\link{Median.test}}, 
  \code{\link{PBIB.test}}, \code{\link{REGW.test}}, \code{\link{SNK.test}},
  \code{\link{waerden.test}}, \code{\link{waller.test}}, \code{\link{plot.group}}
}
\examples{
library(agricolae)
data(sweetpotato)
model<-aov(yield~virus, data=sweetpotato)
comparison <- scheffe.test(model,"virus", group=TRUE,console=TRUE,
main="Yield of sweetpotato\nDealt with different virus")
# Old version scheffe.test()
df<-df.residual(model)
MSerror<-deviance(model)/df
Fc<-anova(model)["virus",4]
out <- with(sweetpotato,scheffe.test(yield, virus, df, MSerror, Fc))
print(out)
}
\keyword{ htest }% at least one, from doc/KEYWORDS

