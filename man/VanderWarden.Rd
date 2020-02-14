\name{waerden.test}
\alias{waerden.test}
%- waerden.test.
\title{ Multiple comparisons. The van der Waerden (Normal Scores) }
\description{
  A nonparametric test for several independent samples.
}
\usage{
waerden.test(y, trt, alpha=0.05, group=TRUE, main=NULL,console=FALSE)
}
\arguments{
  \item{y}{ Variable response }
  \item{trt}{ Treatments }
  \item{alpha}{ Significant level }
  \item{group}{ TRUE or FALSE }
  \item{main}{ Title }
  \item{console}{logical, print output }
}
\details{
The data consist of k samples of possibly unequal sample size.\cr
The post hoc test is using the criterium Fisher's least \cr
significant difference (LSD).
}
\value{
  \item{statistics}{Statistics of the model}
  \item{parameters}{Design parameters}
  \item{means}{Statistical summary of the study variable}
  \item{comparison}{Comparison between treatments}
  \item{groups}{Formation of treatment groups}
}
\references{ Practical Nonparametrics Statistics. W.J. Conover, 1999 }
\author{ Felipe de Mendiburu }
\seealso{
  \code{\link{BIB.test}}, \code{\link{DAU.test}}, \code{\link{duncan.test}},
  \code{\link{durbin.test}}, \code{\link{friedman}}, \code{\link{HSD.test}},
  \code{\link{kruskal}}, \code{\link{LSD.test}}, \code{\link{Median.test}}, 
  \code{\link{PBIB.test}}, \code{\link{REGW.test}}, \code{\link{scheffe.test}}, 
  \code{\link{SNK.test}}, \code{\link{waller.test}}, \code{\link{plot.group}}
}
\examples{
library(agricolae)
# example 1
data(corn)
out1<-with(corn,waerden.test(observation,method,group=TRUE))
print(out1$groups)
plot(out1)
out2<-with(corn,waerden.test(observation,method,group=FALSE))
print(out2$comparison)
# example 2
data(sweetpotato)
out<-with(sweetpotato,waerden.test(yield,virus,alpha=0.01,group=TRUE))
print(out)
}

\keyword{ nonparametric }% at least one, from doc/KEYWORDS

