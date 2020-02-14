\name{friedman}
\alias{friedman}
%- friedman.
\title{ Friedman test and multiple comparison of treatments }
\description{
The data consist of b-blocks mutually independent k-variate random variables 
Xij, i=1,..,b; j=1,..k. The random variable X is in block i and is associated 
with treatment j. It makes the multiple comparison of the Friedman test with 
or without ties. A first result is obtained by friedman.test of R.
}
\usage{
friedman(judge,trt,evaluation,alpha=0.05,group=TRUE,main=NULL,console=FALSE)
}

\arguments{
  \item{judge}{ Identification of the judge in the evaluation }
  \item{trt}{ Treatment }
  \item{evaluation}{ Variable }
  \item{alpha}{ Significant test }
  \item{group}{ TRUE or FALSE }
  \item{main}{ Title }
  \item{console}{logical, print output }
}  

\details{
The post hoc friedman test is using the criterium Fisher's least 
significant difference (LSD)
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
  \code{\link{durbin.test}}, \code{\link{HSD.test}}, \code{\link{kruskal}}, 
  \code{\link{LSD.test}}, \code{\link{Median.test}}, \code{\link{PBIB.test}}, 
  \code{\link{REGW.test}}, \code{\link{scheffe.test}}, \code{\link{SNK.test}},
  \code{\link{waerden.test}}, \code{\link{waller.test}}, \code{\link{plot.group}}
}
\examples{
library(agricolae)
data(grass)
out<-with(grass,friedman(judge,trt, evaluation,alpha=0.05, group=TRUE,console=TRUE,
main="Data of the book of Conover"))
#startgraph
plot(out,variation="IQR")
#endgraph
}
\keyword{ nonparametric }% at least one, from doc/KEYWORDS

