\name{DAU.test}
\alias{DAU.test}
%- DAU.test.
\title{ Finding the Variance Analysis of the Augmented block Design  }
\description{
  Analysis of variance Augmented block and comparison mean adjusted. 
}
\usage{
DAU.test(block, trt, y, method = c("lsd","tukey"),alpha=0.05,group=TRUE,console=FALSE)

}

\arguments{
  \item{block}{ blocks }
  \item{trt}{ Treatment }
  \item{y}{ Response  }
  \item{method}{ Comparison treatments }
  \item{alpha}{ Significant test }
  \item{group}{ TRUE or FALSE }
  \item{console}{logical, print output }
}
\details{
Method of comparison treatment. 
lsd: Least significant difference.
tukey: Honestly significant differente.
}
\value{
  \item{means}{Statistical summary of the study variable}
  \item{parameters}{Design parameters}
  \item{statistics}{Statistics of the model}
  \item{comparison}{Comparison between treatments}
  \item{groups}{Formation of treatment groups}
  \item{SE.difference}{ Standard error of:\cr
  Two Control Treatments\cr
  Two Augmented Treatments\cr
  Two Augmented Treatments(Different Blocks)\cr
  A Augmented Treatment and A Control Treatment\cr 
  }
  \item{vartau}{Variance-covariance matrix of the difference in treatments}   
}
\references{ Federer, W. T. (1956). Augmented (or hoonuiaku) designs. Hawaiian Planters, Record LV(2):191-208. }

\author{ F. de Mendiburu }
\seealso{
  \code{\link{BIB.test}}, \code{\link{duncan.test}}, \code{\link{durbin.test}}, 
  \code{\link{friedman}}, \code{\link{HSD.test}}, \code{\link{kruskal}}, 
  \code{\link{LSD.test}}, \code{\link{Median.test}}, \code{\link{PBIB.test}}, 
  \code{\link{REGW.test}}, \code{\link{scheffe.test}}, \code{\link{SNK.test}},
  \code{\link{waerden.test}}, \code{\link{waller.test}}, \code{\link{plot.group}}
}
\examples{
library(agricolae)
block<-c(rep("I",7),rep("II",6),rep("III",7))
trt<-c("A","B","C","D","g","k","l","A","B","C","D","e","i","A","B","C","D","f","h","j")
yield<-c(83,77,78,78,70,75,74,79,81,81,91,79,78,92,79,87,81,89,96,82)
out<- DAU.test(block,trt,yield,method="lsd", group=TRUE)
print(out$groups)
plot(out)
}
\keyword{ models }

