\name{resampling.model}
\alias{resampling.model}
%- resampling.model.
\title{ Resampling for linear models }
\description{
  This process consists of finding the values of P-value by means of a 
  re-sampling (permutation) process along with the values obtained by variance analysis.
}
\usage{
resampling.model(model,data,k,console=FALSE)
}

\arguments{
  \item{model}{ model in R }
  \item{data}{ data for the study of the model }
  \item{k}{ number of re-samplings }
  \item{console}{logical, print output }
}

\value{
Model solution with resampling.
}

\references{
{Efron, B., Tibshirani, R. (1993) An Introduction to the Boostrap. Chapman and Hall/CRC }
{Phillip I. Good, (2001) Resampling Methods. Birkhauser. Boston . Basel . Berlin } 
}
\author{ Felipe de Mendiburu }

\seealso{\code{\link{simulation.model} } }

\examples{
#example 1 Simple linear regression
library(agricolae)
data(clay)
model<-"ralstonia ~ days"
analysis<-resampling.model(model,clay,k=2,console=TRUE)

#example 2 Analysis of variance: RCD
data(sweetpotato)
model<-"yield~virus"
analysis<-resampling.model(model,sweetpotato,k=2,console=TRUE)

#example 3 Simple linear regression
data(Glycoalkaloids)
model<-"HPLC ~ spectrophotometer"
analysis<-resampling.model(model,Glycoalkaloids,k=2,console=TRUE)

#example 4 Factorial in RCD

data(potato)
potato[,1]<-as.factor(potato[,1])
potato[,2]<-as.factor(potato[,2])
model<-"cutting~variety + date + variety:date"
analysis<-resampling.model(model,potato,k=2,console=TRUE)

}
\keyword{ multivariate }% at least one, from doc/KEYWORDS

