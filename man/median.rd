\name{Median.test}
\alias{Median.test}
%- median.test.
\title{ Median test. Multiple comparisons }
\description{
  A nonparametric test for several independent samples. The median test is designed
  to examine whether several samples came from populations having the same median.
  }
\usage{
Median.test(y,trt,alpha=0.05,correct=TRUE,simulate.p.value = FALSE, group = TRUE, 
main = NULL,console=TRUE)
}
\arguments{
  \item{y}{ Variable response }
  \item{trt}{ Treatments }
  \item{alpha}{ error type I }
  \item{correct}{ a logical indicating whether to apply continuity correction
  when computing the test statistic for 2 groups. The correction will not be bigger
  than the differences themselves. No correction is done if simulate.p.value = TRUE.}
  \item{simulate.p.value}{a logical indicating whether to compute p-values by
  Monte Carlo simulation}
  \item{group}{ TRUE or FALSE }
  \item{main}{ Title }
  \item{console}{logical, print output }
}
\details{
The data consist of k samples of possibly unequal sample size.\cr
Greater: is the number of values that exceed the median of all data and \cr
LessEqual: is the number less than or equal to the median of all data.
}
\value{
  \item{statistics}{Statistics of the model}
  \item{parameters}{Design parameters}
  \item{medians}{Statistical summary of the study variable}
  \item{comparison}{Comparison between treatments}
  \item{groups}{Formation of treatment groups}
}
\references{ Practical Nonparametrics Statistics. W.J. Conover, 1999 }
\author{ Felipe de Mendiburu }
\seealso{
  \code{\link{BIB.test}}, \code{\link{DAU.test}}, \code{\link{duncan.test}},
  \code{\link{durbin.test}}, \code{\link{friedman}}, \code{\link{HSD.test}},
  \code{\link{kruskal}}, \code{\link{LSD.test}}, \code{\link{PBIB.test}}, 
  \code{\link{REGW.test}}, \code{\link{scheffe.test}}, \code{\link{SNK.test}},
  \code{\link{waerden.test}}, \code{\link{waller.test}}, \code{\link{plot.group}}
}
\examples{
library(agricolae)
# example 1
data(corn)
out<-with(corn,Median.test(observation,method,console=FALSE))
z<-bar.err(out$medians,variation = "range",ylim=c(0,120),
           space=2,border=4,col=3,density=10,angle=45)
# example 2
out<-with(corn,Median.test(observation,method,console=FALSE,group=FALSE))
print(out$comparison)
}

\keyword{ nonparametric }% at least one, from doc/KEYWORDS

