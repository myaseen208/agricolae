\name{kurtosis}
\alias{kurtosis}
%- c.kurtosis.
\title{ Finding the Kurtosis coefficient }
\description{
  It obtains the value of the kurtosis for a normally distributed variable.
  The result is similar to SAS.
}
\usage{
kurtosis(x)
}

\arguments{
  \item{x}{ a numeric vector }
}

\value{

  \item{x }{ The kurtosis of x }

}

\seealso{\code{\link{skewness} } }

\examples{
library(agricolae)
x<-c(3,4,5,2,3,4,5,6,4,NA,7)
kurtosis(x)
# value is -0.1517996
}
\keyword{ univar }% at least one, from doc/KEYWORDS

