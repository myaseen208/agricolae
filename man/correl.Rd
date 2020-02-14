\name{correl}
\alias{correl}
%- correl.
\title{ Correlation Coefficient }
\description{
   An exact correlation for ties or without ties.
   Methods of Kendall, Spearman and Pearson.
}
\usage{
correl(x, y, method = "pearson",alternative="two.sided")
}

\arguments{
  \item{x}{ Vector }
  \item{y}{ Vector }
  \item{method}{ "pearson", "kendall", "spearman" }
  \item{alternative}{ "two.sided", "less", "greater" }
}

\value{
The correlation of x,y vector with the statistical value and its probability
}
\references{Numerical Recipes in C. Second Edition. }
\author{ Felipe de Mendiburu }

\seealso{\code{\link{correlation}} }

\examples{
library(agricolae)
data(soil)
with(soil,correl(pH,clay,method="kendall"))
with(soil,correl(pH,clay,method="spearman"))
with(soil,correl(pH,clay,method="pearson"))
}
\keyword{ multivariate }% at least one, from doc/KEYWORDS

