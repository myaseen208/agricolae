\name{zigzag}
\alias{zigzag}
%- zigzag.
\title{ order plot in serpentine }
\description{
  applied to designs: complete block, latin square, graeco, split plot, strip plot, lattice, alpha lattice,
  Augmented block, cyclic, Balanced Incomplete Block and factorial.
}
\usage{
zigzag(outdesign)
}

\arguments{
  \item{outdesign}{ output design}
}
\value{
  \item{fieldbook}{Remuneration of serpentine plots.}
}
\author{ Felipe de Mendiburu }
\seealso{\code{\link{design.ab}}, \code{\link{design.alpha}},\code{\link{design.bib}}, 
\code{\link{design.split} }, \code{\link{design.cyclic} }, \code{\link{design.dau} },
\code{\link{design.graeco}}, \code{\link{design.lattice}}, \code{\link{design.lsd}},
\code{\link{design.rcbd}}, \code{\link{design.strip}} }

\examples{
library(agricolae)
trt<-letters[1:5]
r<-4
outdesign <- design.rcbd(trt,r,seed=9)
fieldbook <- zigzag(outdesign)
}
\keyword{ manip }

