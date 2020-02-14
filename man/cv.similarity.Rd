\name{cv.similarity}
\alias{cv.similarity}
%- cv.similarity.
\title{ Coefficient of the similarity matrix variation }
\description{
  This process consists of finding the coefficient of the distances of 
  similarity of binary tables (1 and 0) as used for scoring molecular
  marker data for presence and absence of PCR amplification products.
}
\usage{
cv.similarity(A)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{ matrix of binary data }
}

\value{
Returns the coefficient of variation of the similarity model
}
\author{ Felipe de Mendiburu }

\seealso{\code{\link{similarity}}, \code{\link{resampling.cv} } }

\examples{
# molecular markers.
library(agricolae)
data(markers)
cv<-cv.similarity(markers)

}
\keyword{ multivariate }% at least one, from doc/KEYWORDS

