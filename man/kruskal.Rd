\name{kruskal}
\alias{kruskal}
%- kruskal.
\title{ Kruskal Wallis test and multiple comparison of treatments. }
\description{
 It makes the multiple comparison with Kruskal-Wallis. The alpha parameter by default is 0.05. Post hoc test is using the criterium Fisher's least significant difference. The adjustment methods include the Bonferroni correction  and others.
}
\usage{
kruskal(y, trt, alpha = 0.05, p.adj=c("none","holm","hommel", 
"hochberg", "bonferroni", "BH", "BY", "fdr"), group=TRUE, main = NULL,console=FALSE)
}
\arguments{
  \item{y}{ response }
  \item{trt}{ treatment }
  \item{alpha}{ level signification }
  \item{p.adj}{ Method for adjusting p values (see p.adjust)  }
  \item{group}{ TRUE or FALSE }
  \item{main}{ Title }
  \item{console}{logical, print output }
}
\details{
  For equal or different repetition.\cr
  For the adjustment methods, see the function p.adjusted.\cr
  p-adj = "none" is t-student.
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
  \code{\link{LSD.test}}, \code{\link{Median.test}}, \code{\link{PBIB.test}}, 
  \code{\link{REGW.test}}, \code{\link{scheffe.test}}, \code{\link{SNK.test}},
  \code{\link{waerden.test}}, \code{\link{waller.test}}, \code{\link{plot.group}}
}
\examples{
library(agricolae)
data(corn)
str(corn)
comparison<-with(corn,kruskal(observation,method,group=TRUE, main="corn"))
comparison<-with(corn,kruskal(observation,method,p.adj="bon",group=FALSE, main="corn"))
}
\keyword{ nonparametric }% at least one, from doc/KEYWORDS

