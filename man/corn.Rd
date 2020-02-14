\name{corn}
\alias{corn}
\docType{data}
\title{ Data of corn }
\description{
  Data from a completely randomized design where four different methods of growing
  corn resulted in various yields per acre on various plots of ground where the 
  four methods were tried. Ordinarily, only one statistical analysis is used, 
  but here we will use the kuskal-wallis test so that a rough comparison may be made
  with the mediasn test.
}
\usage{data(corn)}
\format{
  A data frame with 34 observations on the following 3 variables.
  \describe{
    \item{\code{method}}{a numeric vector}
    \item{\code{observation}}{a numeric vector}
    \item{\code{rx}}{a numeric vector}
  }
}
\details{
  The observations are ranked from the smallest, 77, of rank 1 to the largest 101, 
  of rank N=34. Ties values receive the averarge rank.
  
}
\source{
  Book: Practical Nonparametric Statistics.
}
\references{
  Practical Nonparametrics Statistics. W.J. Conover. Third Edition, 1999.
}
\examples{
data(corn)
str(corn)
}
\keyword{datasets}
