% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/correlation.R
\name{correlation}
\alias{correlation}
\title{Correlation analysis. Methods of Pearson, Spearman, Kendall and Lin}
\usage{
correlation(
  x,
  y = NULL,
  method = c("pearson", "kendall", "spearman", "lin"),
  alternative = "two.sided"
)
}
\arguments{
\item{x}{table, matrix or vector}

\item{y}{table, matrix or vector}

\item{method}{"pearson", "kendall", "spearman", "lin"}

\item{alternative}{"two.sided", "less", "greater"}
}
\value{
The correlation matrix with its probability
}
\description{
It obtains the coefficients of correlation and p-value between all the
variables of a data table. The methods to apply are Pearson, Spearman ,
Kendall and lin's concordance index. In case of not specifying the method,
the Pearson method will be used. The results are similar to SAS.
}
\details{
Parameters equal to function cor()
}
\examples{

library(agricolae)
data(soil)
# example 1
analysis<-correlation(soil[,2:8],method="pearson")
analysis
# Example 2: correlation between pH, variable 2 and other elements from soil.
analysis<-with(soil,correlation(pH,soil[,3:8],method="pearson",alternative="less"))
analysis
# Example 3: correlation between pH and clay method kendall.
with(soil,correlation(pH,clay,method="kendall", alternative="two.sided"))

}
\references{
Lin LI. A concordance correlation coefficient to evaluate
reproducibility.  Biometrics. 1989; 45, 255-268.
}
\seealso{
\code{\link{correl} }
}
\author{
Felipe de Mendiburu
}
\keyword{multivariate}
