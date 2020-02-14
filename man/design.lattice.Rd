\name{design.lattice}
\alias{design.lattice}
%- design.lattice.
\title{ Lattice designs }
\description{
  SIMPLE and TRIPLE lattice designs. It randomizes treatments in k x k lattice.
}
\usage{
design.lattice(trt, r=3, serie = 2, seed = 0, kinds = "Super-Duper",randomization=TRUE)
}

\arguments{
  \item{trt}{ treatments }
  \item{r}{r=2(simple) or r=3(triple) lattice}
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
  \item{statistics }{Design statistics}
  \item{sketch}{Design sketch}
  \item{book}{Fieldbook}
}
\references{ FIELD PLOT TECHNIQUE. Erwin L. LeCLERG. 2nd ed., 1962, 
 Burgess Publishing Company, Minnesota }

\author{ Felipe de Mendiburu }

\seealso{\code{\link{design.ab}}, \code{\link{design.alpha}},\code{\link{design.bib}}, 
\code{\link{design.crd} }, \code{\link{design.cyclic} }, \code{\link{design.dau} },
\code{\link{design.graeco}}, \code{\link{design.split}}, \code{\link{design.lsd}},
\code{\link{design.rcbd}}, \code{\link{design.strip}} }

\examples{
library(agricolae)
# triple lattice
trt<-LETTERS[1:9]
outdesign<-design.lattice(trt,r=3,serie=2) # triple lattice design ( 9 trt)
# simple lattice
trt<-1:100
outdesign<-design.lattice(trt,r=2,serie=3) # simple lattice design, 10x10 
}
\keyword{ design }% at least one, from doc/KEYWORDS

