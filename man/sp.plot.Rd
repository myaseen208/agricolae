\name{sp.plot}
\alias{sp.plot}
%- split.plot.
\title{ Splip-Plot analysis }
\description{
  The variance analysis of a split plot design is divided into two parts: 
  the plot-factor analysis and the sub-plot factor analysis.
}
\usage{
sp.plot(block, pplot, splot, Y)
}

\arguments{
  \item{block}{ replications }
  \item{pplot}{ main-plot Factor}
  \item{splot}{ sub-plot Factor }
  \item{Y}{ Variable, response }
}
\details{
  The split-plot design is specifically suited for a two-factor experiment
  on of the factors is assigned to main plot (main-plot factor), the second 
  factor, called the subplot factor, is assigned into subplots.
}
\value{
ANOVA: Splip plot analysis
}
\references{ Statistical procedures for agricultural research.
 Kwanchai A. Gomez, Arturo A. Gomez. Second Edition. 1984.
 }
\author{ Felipe de Mendiburu  }
\seealso{\code{\link{ssp.plot}}, \code{\link{strip.plot}}, \code{\link{design.split}}, 
\code{\link{design.strip} } }
\examples{
library(agricolae)
data(plots)
model<-with(plots,sp.plot(block,A,B,yield))
# with aov
plots[,1]<-as.factor(plots[,1])
AOV <- aov(yield ~ block + A*B + Error(block/A),data=plots)
summary(AOV)
}
\keyword{ models }% at least one, from doc/KEYWORDS




