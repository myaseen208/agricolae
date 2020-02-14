\name{design.crd}
\alias{design.crd}
%- design.crd.
\title{ Completely Randomized Design }
\description{
  It generates completely a randomized design with equal or different repetition.
  "Random" uses the methods of number generation in R.
  The seed is by set.seed(seed, kinds).

}
\usage{
design.crd(trt, r, serie = 2, seed = 0, kinds = "Super-Duper",randomization=TRUE)
}

\arguments{
  \item{trt}{ Treatments}
  \item{r}{ Replications }
  \item{serie}{ number plot, 1: 11,12; 2: 101,102; 3: 1001,1002 }
  \item{seed}{ seed }
  \item{kinds}{ method for to randomize }
  \item{randomization}{ TRUE or FALSE - randomize}
}
\details{
  kinds <- c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper",
  "Mersenne-Twister", "Knuth-TAOCP", "user-supplied",  "Knuth-TAOCP-2002",
  "default" )
}
\value{
  \item{parameters}{Design parameters}
  \item{book}{Fieldbook}
}
\references{ Introduction to Experimental Statistics. 
Ching Chun Li. McGraw-Hill Book Company, INC, New. York, 1964 }
\author{ Felipe de Mendiburu }

\seealso{\code{\link{design.ab}}, \code{\link{design.alpha}},\code{\link{design.bib}}, 
\code{\link{design.split} }, \code{\link{design.cyclic} }, \code{\link{design.dau} },
\code{\link{design.graeco}}, \code{\link{design.lattice}}, \code{\link{design.lsd}},
\code{\link{design.rcbd}}, \code{\link{design.strip}} }

\examples{
library(agricolae)
trt <-c("CIP-101","CIP-201","CIP-301","CIP-401","CIP-501")
r <-c(4,3,5,4,3)
# seed = 12543
outdesign1 <-design.crd(trt,r,serie=2,2543,"Mersenne-Twister")
book1<-outdesign1
# no seed
outdesign2 <-design.crd(trt,r,serie=3)
print(outdesign2$parameters)
book2<-outdesign2
# write to hard disk
# write.table(book1,"crd.txt", row.names=FALSE, sep="\t")
# file.show("crd.txt")
}
\keyword{ design }% at least one, from doc/KEYWORDS

