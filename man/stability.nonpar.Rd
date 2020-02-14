\name{stability.nonpar}
\alias{stability.nonpar}
%- stability.nonpar.
\title{ Nonparametric stability analysis }
\description{
  A method based on the statistical ranges of the study variable per 
  environment for the stability analysis.
}
\usage{
stability.nonpar(data, variable = NULL, ranking = FALSE, console=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ First column the genotypes following environment  }
  \item{variable}{ Name of variable }
  \item{ranking}{logical, print ranking }
  \item{console}{logical, print output }
}
\value{
  \item{ranking}{ data frame }
  \item{statistics}{ Statistical analysis chi square test }
}
\references{
Haynes K G, Lambert D H, Christ B J, Weingartner D P, Douches D S,
Backlund J E, Fry W and Stevenson W. 1998. Phenotypic stability of resistance
to late blight in potato clones evaluated at eight sites in the United States
American Journal Potato Research 75, pag 211-217.
 }
\author{ Felipe de Mendiburu}

\seealso{\code{\link{stability.par} } }

\examples{
library(agricolae)
data(haynes)
stability.nonpar(haynes,"AUDPC",ranking=TRUE,console=TRUE)
# Example 2
data(CIC)
data1<-CIC$comas[,c(1,6,7,17,18)]
data2<-CIC$oxapampa[,c(1,6,7,19,20)]
cic <- rbind(data1,data2)

means <- by(cic[,5], cic[,c(2,1)], function(x) mean(x,na.rm=TRUE))
means <-as.data.frame(means[,])
cic.mean<-data.frame(genotype=row.names(means),means)
cic.mean<-delete.na(cic.mean,"greater")
out<-stability.nonpar(cic.mean)
out$ranking
out$statistics
}
\keyword{ nonparametric }% at least one, from doc/KEYWORDS

