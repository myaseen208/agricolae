\name{design.rcbd}
\alias{design.rcbd}
%- design.rcbd.
\title{ Randomized Complete Block Design }
\description{
  It generates Randomized Complete Block Design.
  "Random" uses the methods of number generation in R.
  The seed is by set.seed(seed, kinds).
}
\usage{
design.rcbd(trt, r, serie = 2, seed = 0, kinds = "Super-Duper", first=TRUE,
continue=FALSE,randomization=TRUE )
}

\arguments{
  \item{trt}{ Treatments}
  \item{r}{ Replications or blocks }
  \item{serie}{ number plot, 1: 11,12; 2: 101,102; 3: 1001,1002 }
  \item{seed}{ seed }
  \item{kinds}{ method for to randomize }
  \item{first}{ TRUE or FALSE - randomize rep 1}
  \item{continue}{ TRUE or FALSE, continuous numbering of plot}
  \item{randomization}{ TRUE or FALSE - randomize}
}
\details{
  kinds <- c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper",
  "Mersenne-Twister", "Knuth-TAOCP", "user-supplied",  "Knuth-TAOCP-2002",
  "default" )
}
\value{
  \item{parameters}{Design parameters}
  \item{sketch}{Design sketch}
  \item{book}{Fieldbook}
}
\references{ Introduction to Experimental Statistics. 
Ching Chun Li. McGraw-Hill Book Company, INC, New. York, 1964 }
\author{ Felipe de Mendiburu }

\seealso{\code{\link{design.ab}}, \code{\link{design.alpha}},\code{\link{design.bib}}, 
\code{\link{design.crd} }, \code{\link{design.cyclic} }, \code{\link{design.dau} },
\code{\link{design.graeco}}, \code{\link{design.lattice}}, \code{\link{design.lsd}},
\code{\link{design.split}}, \code{\link{design.strip}} }

\examples{
library(agricolae)
# 5 treatments and 6 blocks
trt<-c("A","B","C","D","E")
outdesign <-design.rcbd(trt,6,serie=2,986,"Wichmann-Hill") # seed = 986
book <-outdesign$book # field book
# write in hard disk
# write.table(book,"rcbd.txt", row.names=FALSE, sep="\t")
# file.show("rcbd.txt")
# Plots in field model ZIGZAG
fieldbook <- zigzag(outdesign)
print(outdesign$sketch)
print(matrix(fieldbook[,1],byrow=TRUE,ncol=5))
# continuous numbering of plot
outdesign <-design.rcbd(trt,6,serie=0,continue=TRUE)
head(outdesign$book)
}
\keyword{ design }% at least one, from doc/KEYWORDS

