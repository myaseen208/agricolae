\name{ssp.plot}
\alias{ssp.plot}
%- ssp.plot.
\title{ Split-split-Plot analysis }
\description{
  The variance analysis of a split-split plot design is divided into three parts: 
  the main-plot, subplot and sub-subplot analysis.
}
\usage{
ssp.plot(block, pplot, splot, ssplot, Y)
}

\arguments{
  \item{block}{ replications }
  \item{pplot}{ Factor main plot }
  \item{splot}{ Factor subplot }
  \item{ssplot}{ Factor sub-subplot}
  \item{Y}{ Variable, response }
}
\details{
  The split-split-plot design is an extension of the split-plot design
  to accommodate a third factor: one factor in main-plot, other in subplot 
  and the third factor in sub-subplot
}
\value{
ANOVA: Splip Split plot analysis
}
\references{ Statistical procedures for agricultural research.
 Kwanchai A. Gomez, Arturo A. Gomez. Second Edition. 1984.
 }
\author{ Felipe de Mendiburu  }
\seealso{\code{\link{sp.plot}}, \code{\link{strip.plot}}, \code{\link{design.split}}, 
\code{\link{design.strip} } }
\examples{
# Statistical procedures for agricultural research, pag 143
# Grain Yields of Three Rice Varieties Grown under 
#Three Management practices and Five Nitrogen levels; in a
#split-split-plot design with nitrogen as main-plot, 
#management practice as subplot, and variety as sub-subplot 
#factores, with three replications.
library(agricolae)
f <- system.file("external/ssp.csv", package="agricolae")
ssp<-read.csv(f)
model<-with(ssp,ssp.plot(block,nitrogen,management,variety,yield))
gla<-model$gl.a; glb<-model$gl.b; glc<-model$gl.c
Ea<-model$Ea; Eb<-model$Eb; Ec<-model$Ec
par(mfrow=c(1,3),cex=0.6)
out1<-with(ssp,LSD.test(yield,nitrogen,gla,Ea,console=TRUE))
out2<-with(ssp,LSD.test(yield,management,glb,Eb,console=TRUE))
out3<-with(ssp,LSD.test(yield,variety,glc,Ec,console=TRUE))
plot(out1,xlab="Nitrogen",las=1,variation="IQR")
plot(out2,xlab="Management",variation="IQR")
plot(out3,xlab="Variety",variation="IQR")
# with aov
AOV<-aov(yield ~  block + nitrogen*management*variety + Error(block/nitrogen/management),data=ssp)
summary(AOV)
}
\keyword{ models }% at least one, from doc/KEYWORDS




