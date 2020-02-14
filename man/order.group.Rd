\name{order.group}
\alias{order.group}
%- order.group.
\title{ Ordering the treatments according to the multiple comparison }
\description{
  This function allows us to compare the treatments averages or the adding of 
  their ranges with the minimal significant difference which can vary from one
  comparison to another one.
}
\usage{
order.group(trt, means, N, MSerror, Tprob, std.err, parameter=1, snk=0, 
DFerror=NULL,alpha=NULL,sdtdif=NULL,vartau=NULL,console)
}

\arguments{
  \item{trt}{ Treatments }
  \item{means}{ Means of treatment }
  \item{N}{ Replications }
  \item{MSerror}{ Mean square error }
  \item{Tprob}{ minimum value for the comparison }
  \item{std.err}{ standard error }
  \item{parameter}{ Constante 1 (Sd), 0.5 (Sx) }
  \item{snk}{ Constante = 1 (Student Newman Keuls) }
  \item{DFerror}{ Degrees of freedom of the experimental error }
  \item{alpha}{ Level of risk for the test  } 
  \item{sdtdif}{ standard deviation of difference in BIB  }
  \item{vartau}{ matrix var-cov in PBIB  } 
  \item{console}{logical, print output }
}

\details{
This function was changed by orderPvalue function that use agricolae.
Now the grouping in agricolae is with the probability of the treatments 
differences and alpha level.
}

\note{
It is considered 81 labels as maximum for the formation of groups, 
greater number will not have label.
}

\value{
The output is data frame.

  \item{trt}{ Treatment Levels, Factor }
  \item{means}{ height, Numeric }
  \item{M}{ groups levels, Factor }
  \item{N}{ replications, Numeric }
  \item{std.err}{ Standard error, Numeric }
}
\author{ Felipe de Mendiburu }
\seealso{\code{\link{orderPvalue} } }
\examples{
library(agricolae)
treatments <- c("A","B","C","D","E","F")
means<-c(20,40,35,72,49,58)
std.err<-c(1.2, 2, 1.5, 2.4, 1, 3.1)
replications <- c(4,4,3,4,3,3)
MSerror <- 55.8
value.t <- 2.1314
groups<-order.group(treatments,means,replications,MSerror,value.t,std.err,console=FALSE)
print(groups)
}
\keyword{ manip }% at least one, from doc/KEYWORDS

