\name{strip.plot}
\alias{strip.plot}
%- Strip.plot.
\title{ Strip-Plot analysis }
\description{
  The variance analysis of a strip-plot design is divided into three parts: 
  the horizontal-factor analysis, the vertical-factor analysis, and the 
  interaction analysis.
}
\usage{
strip.plot(BLOCK, COL, ROW, Y)
}

\arguments{
  \item{BLOCK}{ replications }
  \item{COL}{ Factor column }
  \item{ROW}{ Factor row }
  \item{Y}{ Variable, response }
}
\details{
  The strip-plot design is specifically suited for a two-factor experiment
  in which the desired precision for measuring the interaction effects
  between the two factors is higher than that for measuring the main efect
  two factors
}
\value{
Data and analysis of the variance of the strip plot design. 
}
\references{ Statistical procedures for agricultural research.
 Kwanchai A. Gomez, Arturo A. Gomez. Second Edition. 1984.
 }
\author{ Felipe de Mendiburu  }
\seealso{\code{\link{ssp.plot}}, \code{\link{sp.plot}}, \code{\link{design.split}}, 
\code{\link{design.strip} } }
\examples{
# Yield
library(agricolae)
data(huasahuasi)
YIELD<-huasahuasi$YIELD
market <- YIELD$y1da + YIELD$y2da
non_market <- YIELD$y3da
yield <- market + non_market
model<-with(YIELD,strip.plot(block, clon, trt, yield))
out1<-with(YIELD,LSD.test(yield,clon,model$gl.a,model$Ea))
oldpar<-par(mar=c(3,8,1,1),cex=0.8)
plot(out1,xlim=c(0,80),horiz=TRUE,las=1)
out2<-with(YIELD,LSD.test(yield,trt,model$gl.b,model$Eb))
plot(out2,xlim=c(0,80),horiz=TRUE,las=1)
par(oldpar)
}
\keyword{ models }% at least one, from doc/KEYWORDS
