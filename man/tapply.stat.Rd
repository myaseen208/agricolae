% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tapply.stat.R
\name{tapply.stat}
\alias{tapply.stat}
\title{Statistics of data grouped by factors}
\usage{
tapply.stat(y, x, stat = "mean")
}
\arguments{
\item{y}{data.frame variables}

\item{x}{data.frame factors}

\item{stat}{Method}
}
\value{
Statistics of quantitative variables by categorical variables.
}
\description{
This process lies in finding statistics which consist of more than one
variable, grouped or crossed by factors. The table must be organized by
columns between variables and factors.
}
\examples{

library(agricolae)
# case of 1 single factor
data(sweetpotato)
tapply.stat(sweetpotato[,2],sweetpotato[,1],mean)
with(sweetpotato,tapply.stat(yield,virus,sd))
with(sweetpotato,tapply.stat(yield,virus,function(x) max(x)-min(x)))
with(sweetpotato,tapply.stat(yield,virus,
function(x) quantile(x,0.75,6)-quantile(x,0.25,6)))
# other case
data(cotton)
with(cotton,tapply.stat(yield,cotton[,c(1,3,4)],mean))
with(cotton,tapply.stat(yield,cotton[,c(1,4)],max))
# Height of pijuayo
data(growth)
with(growth,tapply.stat(height, growth[,2:1], function(x) mean(x,na.rm=TRUE)))

}
\author{
Felipe de Mendiburu
}
\keyword{univar}
