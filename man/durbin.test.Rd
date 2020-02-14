\name{durbin.test}
\alias{durbin.test}
%- durbin.
\title{ Durbin test and multiple comparison of treatments }
\description{
  A multiple comparison of the Durbin test for the balanced incomplete 
  blocks for sensorial or categorical evaluation. It forms groups according
  to the demanded ones for level of significance (alpha); by default, 0.05.
}
\usage{
durbin.test(judge, trt, evaluation, alpha = 0.05, group =TRUE, 
main = NULL, console=FALSE)
}
\arguments{
  \item{judge}{ Identification of the judge in the evaluation }
  \item{trt}{ Treatments }
  \item{evaluation}{ variable }
  \item{alpha}{ level of significant }
  \item{group}{ TRUE or FALSE }
  \item{main}{ Title }
  \item{console}{logical, print output }
}
\details{
The post hoc test is using the criterium Fisher's least significant difference.
}
\value{
  \item{statistics}{Statistics of the model}
  \item{parameters}{Design parameters}
  \item{means}{Statistical summary of the study variable}
  \item{rank}{rank table of the study variable}
  \item{comparison}{Comparison between treatments}
  \item{groups}{Formation of treatment groups}
}
\references{ Practical Nonparametrics Statistics. W.J. Conover, 1999
Nonparametric Statistical Methods. Myles Hollander and Douglas A. Wofe, 1999}
\author{ Felipe de Mendiburu }
\seealso{
  \code{\link{BIB.test}}, \code{\link{DAU.test}}, \code{\link{duncan.test}},
  \code{\link{friedman}}, \code{\link{HSD.test}}, \code{\link{kruskal}}, 
  \code{\link{LSD.test}}, \code{\link{Median.test}}, \code{\link{PBIB.test}}, 
  \code{\link{REGW.test}}, \code{\link{scheffe.test}}, \code{\link{SNK.test}},
  \code{\link{waerden.test}}, \code{\link{waller.test}}, \code{\link{plot.group}}
}
\examples{
library(agricolae)
# Example 1. Conover, pag 391
person<-gl(7,3)
variety<-c(1,2,4,2,3,5,3,4,6,4,5,7,1,5,6,2,6,7,1,3,7)
preference<-c(2,3,1,3,1,2,2,1,3,1,2,3,3,1,2,3,1,2,3,1,2)
out<-durbin.test(person,variety,preference,group=TRUE,console=TRUE,
main="Seven varieties of ice cream manufacturer")
#startgraph
bar.group(out$groups,horiz=TRUE,xlim=c(0,10),density=4,las=1)
#endgraph
# Example 2. Myles Hollander, pag 311
# Source: W. Moore and C.I. Bliss. 1942
day<-gl(7,3)
chemical<-c("A","B","D","A","C","E","C","D","G","A","F","G","B","C","F",
 "B","E","G","D","E","F")
toxic<-c(0.465,0.343,0.396,0.602,0.873,0.634,0.875,0.325,0.330,0.423,0.987,
0.426,0.652,1.142,0.989,0.536,0.409,0.309,0.609,0.417,0.931)
out<-durbin.test(day,chemical,toxic,group=TRUE,console=TRUE,
main="Logarithm of Toxic Dosages")
plot(out)
}
\keyword{ nonparametric }% at least one, from doc/KEYWORDS

