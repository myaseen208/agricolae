\name{index.bio}
\alias{index.bio}
\title{ Biodiversity Index }
\description{
Scientists use a formula called the biodiversity index to describe the amount of
species diversity in a given area.
}
\usage{
index.bio(data, method = c("Margalef", "Simpson.Dom", "Simpson.Div", 
"Berger.Parker", "McIntosh", "Shannon"), level=95, nboot=100, console=TRUE)
}
\arguments{
  \item{data}{ number of specimens }
  \item{method}{ Describe method bio-diversity }
  \item{level}{ Significant level }
  \item{nboot}{ size bootstrap }
  \item{console}{ output console TRUE}
}
\details{
  method bio-diversity.
  "Margalef"
  "Simpson.Dom"
  "Simpson.Div"
  "Berger.Parker"
  "McIntosh"
  "Shannon"
}
\value{
Index and confidence intervals.
}
\references{Magurran, A.E. (1988) Ecological diversity and its measurement. 
Princeton University Press Efron, B., Tibshirani, R. (1993) An Introduction to 
the Boostrap. Chapman and Hall/CRC 
}
\author{ Felipe de Mendiburu }
\examples{
library(agricolae)
data(paracsho)
# date 22-06-05 and treatment CON = application with insecticide
specimens <- paracsho[1:10,6]
output1 <- index.bio(specimens,method="Simpson.Div",level=95,nboot=100)
output2 <- index.bio(specimens,method="Shannon",level=95,nboot=100)
rbind(output1, output2)
}
\keyword{ univar }
