\name{vark}
\alias{vark}
%- vark.
\title{ Variance K, ties, Kendall}
\description{
  The Kendall method in order to find the K variance.
}
\usage{
vark(x, y)
}

\arguments{
  \item{x}{ Vector }
  \item{y}{ vector }
}
\details{
Script in C to R.
}
\value{
variance of K for Kendall's tau
}
\references{Numerical Recipes in C. Second Edition. }
\author{ Felipe de Mendiburu }

\seealso{ cor.matrix, cor.vector, cor.mv }
\examples{
library(agricolae)
x <-c(1,1,1,4,2,2,3,1,3,2,1,1,2,3,2,1,1,2,1,2)
y <-c(1,1,2,3,4,4,2,1,2,3,1,1,3,4,2,1,1,3,1,2)
vark(x,y)
}
\keyword{ nonparametric }% at least one, from doc/KEYWORDS

