\name{skewness}
\alias{skewness}
%- c.skewness.
\title{ Finding the skewness coefficient }
\description{
  It returns the skewness of a distribution. It is similar to SAS.
}
\usage{
skewness(x)
}

\arguments{
  \item{x}{a numeric vector}
}

\value{
The skewness of x.
}

\seealso{\code{\link{kurtosis} } }

\examples{
library(agricolae)
x<-c(3,4,5,2,3,4,NA,5,6,4,7)
skewness(x)
# value is 0,3595431, is slightly asimetrica (positive) to the right
}
\keyword{ univar }% at least one, from doc/KEYWORDS

