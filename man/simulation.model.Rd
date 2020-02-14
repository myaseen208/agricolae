\name{simulation.model}
\alias{simulation.model}
%- simulation.model.
\title{ Simulation of the linear model under normality }
\description{
  This process consists of validating the variance analysis results 
  using a simulation process of the experiment.
  The validation consists of comparing the calculated values of each source
  of variation of the simulated data with respect to the calculated values
  of the original data. If in more than 50 percent of the cases they are higher
  than the real one, then it is considered favorable and the probability
  reported by the ANOVA is accepted, since the P-Value is the probability of
  (F > F.value).
}
\usage{
simulation.model(model,file, categorical = NULL,k,console=FALSE)
}

\arguments{
  \item{model}{ Model in R}
  \item{file}{ Data for the study of the model}
  \item{categorical}{ position of the columns of the data that correspond
   to categorical variables }
  \item{k}{ Number of simulations }
  \item{console}{logical, print output }
}
\value{
  \item{model}{ ouput linear model, lm}
  \item{simulation}{ anova simulation}
}
\author{ Felipe de Mendiburu }

\seealso{\code{\link{resampling.model} } }

\examples{
library(agricolae)
#example 1
data(clay)
model<-"ralstonia ~ days"
simulation.model(model,clay,k=15,console=TRUE)
#example 2
data(sweetpotato)
model<-"yield~virus"
simulation.model(model,sweetpotato,categorical=1,k=15,console=TRUE)
#example 3
data(Glycoalkaloids)
model<-"HPLC ~ spectrophotometer"
simulation.model(model,Glycoalkaloids,k=15,console=TRUE)
#example 4
data(potato)
model<-"cutting~date+variety"
simulation.model(model,potato,categorical=c(1,2,3),k=15,console=TRUE)

}
\keyword{ models }% at least one, from doc/KEYWORDS

