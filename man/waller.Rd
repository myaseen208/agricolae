\name{waller}
\alias{waller}
%- Waller.
\title{ Computations of Bayesian t-values for multiple comparisons }
\description{
A Bayes rule for the symmetric multiple comparisons problem.  
}
\usage{
waller(K, q, f, Fc)
}

\arguments{
  \item{K}{ Is the loss ratio between type I and type II error }
  \item{q}{ Numerator Degrees of freedom }
  \item{f}{ Denominator Degrees of freedom }
  \item{Fc}{ F ratio from an analysis of variance }
}
\details{
K-RATIO (K): value specifies the Type 1/Type 2 error seriousness ratio for 
the Waller-Duncan test. Reasonable values for KRATIO are 50, 100, and 500,
which roughly correspond for the two-level case to ALPHA levels of 0.1, 0.05,
and 0.01. By default, the procedure uses the default value of 100. 
}
\value{
Waller value for the Waller and Duncan test.
}
\references{ 
Waller, R. A. and Duncan, D. B. (1969).
A Bayes Rule for the Symmetric Multiple Comparison Problem,
Journal of the American Statistical Association 64, pages 1484-1504. 

Waller, R. A. and Kemp, K. E. (1976)
Computations of Bayesian t-Values for Multiple Comparisons,
Journal of Statistical Computation and Simulation, 75, pages 169-172.

Principles and procedures of statistics a biometrical approach
Steel & Torry & Dickey. Third Edition 1997.
}
\author{ Felipe de Mendiburu }

\seealso{\code{\link{waller.test}}}

\examples{
# Table Duncan-Waller K=100, F=1.2 pag 649 Steel & Torry
library(agricolae)
K<-100
Fc<-1.2
q<-c(8,10,12,14,16,20,40,100)
f<-c(seq(4,20,2),24,30,40,60,120)
n<-length(q)
m<-length(f)
W.D <-rep(0,n*m)
dim(W.D)<-c(n,m)
for (i in 1:n) {
for (j in 1:m) {
W.D[i,j]<-waller(K, q[i], f[j], Fc)
}}
W.D<-round(W.D,2)
dimnames(W.D)<-list(q,f)
print(W.D)

}
\keyword{ distribution }% at least one, from doc/KEYWORDS
