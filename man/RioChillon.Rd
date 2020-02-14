\name{RioChillon}
\alias{RioChillon}
\docType{data}
\title{ Data and analysis Mother and baby trials}
\description{
  Mother/Baby Trials allow farmers and researchers to test best-bet
  technologies or new cultivars. Evaluation of advanced Clones of potato
  in the Valley of Rio Chillon - PERU (2004)
}
\usage{data(RioChillon)}
\format{
 The format is list of 2:\cr
 1. mother: data.frame:	30 obs. of  3 variables:\cr
 -  block (3 levels)\cr
 -  clon (10 levels)\cr
 -  yield (kg.)\cr
 2. babies: data.frame:	90 obs. of  3 variables:\cr
 -  farmer (9 levels)\cr
 -  clon (10 levels)\cr
 -  yield (kg.)
}
\details{
 1. Replicated researcher-managed "mother trials" with typically 10 treatments
  evaluated in small plots.\cr
 2. Unreplicated "baby trials" with 10 treatments evaluated in large plots.\cr
 3. The "baby trials" with a subset of the treatments in the mother trial.
}
\source{
  Experimental field.
}
\references{
  International Potato Center. CIP - Lima Peru.  
}
\examples{
# Analisys the Mother/Baby Trial Design
library(agricolae)
data(RioChillon)
# First analysis the Mother Trial Design.
model<-aov(yield ~ block + clon, RioChillon$mother)
anova(model)
cv.model(model)
comparison<-with(RioChillon$mother,LSD.test(yield,clon, 18, 4.922, group=TRUE))
# Second analysis the babies Trial.
comparison<-with(RioChillon$babies,friedman(farmer,clon, yield, group=TRUE))
# Third
# The researcher makes use of data from both mother and baby trials and thereby obtains
# information on suitability of new technologies or cultivars
# for different agro-ecologies and acceptability to farmers.
 
}
\keyword{datasets}
