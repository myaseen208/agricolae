\name{carolina}
\alias{carolina}
%- carolina.
\title{ North Carolina Designs I, II and III }
\description{
  Statistic analysis of the Carolina I, II and III genetic designs.
}
\usage{
carolina(model,data)
}
\arguments{
  \item{model}{ Constant }
  \item{data}{ Data frame }
}
\details{
model = 1,2 and 3 is I, II and III see carolina1,2 and 3.
}
\value{
  \item{model}{ model analysis (I, II or III) of caroline design }
  and variance and additive variance of male, female and male.female interaction.
  }

\references{ Biometrical Methods in Quantitative Genetic Analysis,
             Singh, Chaudhary. 1979 }
\author{ Felipe de Mendiburu }

\seealso{\code{\link{DC}} }
\examples{

library(agricolae)
data(DC)
carolina1 <- DC$carolina1
# str(carolina1)
output<-carolina(model=1,carolina1)
output[][-1]

carolina2 <- DC$carolina2
# str(carolina2)
majes<-subset(carolina2,carolina2[,1]==1)
majes<-majes[,c(2,5,4,3,6:8)]
output<-carolina(model=2,majes[,c(1:4,6)])
output[][-1]

carolina3 <- DC$carolina3
# str(carolina3)
output<-carolina(model=3,carolina3)
output[][-1]
}
\keyword{ models }% at least one, from doc/KEYWORDS

