\name{lastC}
\alias{lastC}
%- lastC.
\title{ Setting the last character of a chain  }
\description{
  A special function for the group of treatments in the multiple comparison tests. 
  Use plot.group.
}
\usage{
lastC(x)
}

\arguments{
  \item{x}{ letters }
}
\value{
  \item{x }{Returns the last character of a string}
}
\author{ Felipe de Mendiburu }

\seealso{\code{\link{plot.group} } }

\examples{
library(agricolae)
x<-c("a","ab","b","c","cd")
lastC(x)
# "a" "b" "b" "c" "d"

}
\keyword{ manip }% /KEYWORDS

