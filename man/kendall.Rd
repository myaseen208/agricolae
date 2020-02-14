\name{kendall}
\alias{kendall}
%- kendall.
\title{ Correlation of Kendall }
\description{
  Correlation of Kendall two set. Compute exact p-value with ties.
}
\usage{
kendall(data1, data2)
}
\arguments{
  \item{data1}{ vector }
  \item{data2}{ vector }
}

\value{
The correlation of data1, data2 vector with the statistical value and its probability
}
\references{ Numerical Recipes in C. Second Edition. Pag 634}
\author{ Felipe de Mendiburu }

\seealso{\code{\link{correlation}} }

\examples{
library(agricolae)
x <-c(1,1,1,4,2,2,3,1,3,2,1,1,2,3,2,1,1,2,1,2)
y <-c(1,1,2,3,4,4,2,1,2,3,1,1,3,4,2,1,1,3,1,2)
kendall(x,y)
}
\keyword{ nonparametric }% at least one, from doc/KEYWORDS

