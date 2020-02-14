\name{orderPvalue}
\alias{orderPvalue}
%- orderPvalue.
\title{ Grouping the treatments averages in a comparison with a minimum value  }
\description{
  When there are treatments and their respective values, these can be 
  compared with a minimal difference of meaning.
}
\usage{
orderPvalue(treatment, means, alpha, pvalue, console)
}

\arguments{
  \item{treatment}{ treatment }
  \item{means}{ means of treatment }
  \item{alpha}{ Alpha value, significante value to comparison }
  \item{pvalue}{ Matrix of probabilities to comparison }
  \item{console}{logical, print output }
}
\value{
The means and groups for treatments.
}
\note{
It is considered 81 labels as maximum for the formation of groups, 
greater number will not have label.
}
\author{ Felipe de Mendiburu }

\examples{
library(agricolae)
treatments <- c("A","B","C")
means<-c(2,5,3)
alpha <- 0.05
pvalue<-matrix(1,nrow=3,ncol=3)
pvalue[1,2]<-pvalue[2,1]<-0.03
pvalue[1,3]<-pvalue[3,1]<-0.10
pvalue[2,3]<-pvalue[3,2]<-0.06
out<-orderPvalue(treatments,means,alpha,pvalue,console=TRUE)
barplot(out[,1],names.arg = row.names(out),col=colors()[84:87])
legend("topright",as.character(out$groups),pch=15,col=colors()[84:87],box.col=0)
}
\keyword{ manip }% at least one, from doc/KEYWORDS

