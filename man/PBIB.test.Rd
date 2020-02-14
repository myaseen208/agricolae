\name{PBIB.test}
\alias{PBIB.test}

\title{ Analysis of the Partially Balanced Incomplete Block Design  }
\description{
  Analysis of variance PBIB and comparison mean adjusted.
  Applied to resoluble designs: Lattices and alpha design.
}
\usage{
PBIB.test(block,trt,replication,y,k, method=c("REML","ML","VC"), 
test = c("lsd","tukey"), alpha=0.05, console=FALSE, group=TRUE)
}
\arguments{
  \item{block}{ blocks }
  \item{trt}{ Treatment }
  \item{replication}{ Replication }  
  \item{y}{ Response  }
  \item{k}{ Block size  }  
  \item{method}{ Estimation method: REML, ML and VC }
  \item{test}{ Comparison treatments }
  \item{alpha}{ Significant test }
  \item{console}{logical, print output }
  \item{group}{logical, groups }
}
\details{
Method of comparison treatment. 
lsd: least significant difference.
tukey: Honestly significant difference.
Estimate: specifies the estimation method for the covariance parameters.
The REML is the default method. The REML specification performs residual (restricted) maximum likelihood, and 
The ML specification performs maximum likelihood, and 
the VC specifications apply only to variance component models. 
}
\value{
  \item{ANOVA}{Analysis of variance }
  \item{method}{Estimation method: REML, ML and VC}
  \item{parameters}{Design parameters}  
  \item{statistics}{Statistics of the model}
  \item{model}{Object: estimation model }    
  \item{Fstat}{Criterion AIC and BIC}
  \item{comparison}{Comparison between treatments}
  \item{means}{Statistical summary of the study variable}
  \item{groups}{Formation of treatment groups} 
  \item{vartau}{Variance-Covariance Matrix}   
}
\references{
1. Iterative Analysis of Generalizad Lattice Designs.
 E.R. Williams (1977) Austral J. Statistics 19(1) 39-42. 
 
2. Experimental design. Cochran and Cox. Second edition. 
Wiley Classics Library Edition published 1992
} 
\author{ F. de Mendiburu }
\seealso{
  \code{\link{BIB.test}}, \code{\link{DAU.test}}, \code{\link{duncan.test}},
  \code{\link{durbin.test}}, \code{\link{friedman}}, \code{\link{HSD.test}},
  \code{\link{kruskal}}, \code{\link{LSD.test}}, \code{\link{Median.test}}, 
  \code{\link{REGW.test}}, \code{\link{scheffe.test}}, \code{\link{SNK.test}},
  \code{\link{waerden.test}}, \code{\link{waller.test}}, \code{\link{plot.group}}
}
\examples{
require(agricolae)
# alpha design 
Genotype<-c(paste("gen0",1:9,sep=""),paste("gen",10:30,sep=""))
ntr<-length(Genotype)
r<-2
k<-3
s<-10
obs<-ntr*r
b <- s*r
book<-design.alpha(Genotype,k,r,seed=5)
book$book[,3]<- gl(20,3)
dbook<-book$book
# dataset
yield<-c(5,2,7,6,4,9,7,6,7,9,6,2,1,1,3,2,4,6,7,9,8,7,6,4,3,2,2,1,1,2,
     1,1,2,4,5,6,7,8,6,5,4,3,1,1,2,5,4,2,7,6,6,5,6,4,5,7,6,5,5,4)
rm(Genotype)
# not run
# analysis
# require(nlme) # method = REML or LM in PBIB.test and require(MASS) method=VC
model <- with(dbook,PBIB.test(block, Genotype, replication, yield, k=3, method="VC"))
# model$ANOVA
# plot(model,las=2)
}
\keyword{ models }
