\name{design.lsd}
\alias{design.lsd}
%- design.lsd.
\title{ Latin Square Design }
\description{
  It generates Latin Square Design.
  "Random" uses the methods of number generation in R.
  The seed is by set.seed(seed, kinds).
}
\usage{
design.lsd(trt, serie = 2, seed = 0, kinds = "Super-Duper",first=TRUE,randomization=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{trt}{ Treatments }
  \item{serie}{ number plot, 1: 11,12; 2: 101,102; 3: 1001,1002 }
  \item{seed}{ seed }
  \item{kinds}{ method for to randomize }
  \item{first}{ TRUE or FALSE - randomize rep 1}
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
Ching Chun Li. McGraw-Hill Book Company, INC, New. York, 1969 }
\author{ Felipe de Mendiburu }

\seealso{\code{\link{design.ab}}, \code{\link{design.alpha}},\code{\link{design.bib}}, 
\code{\link{design.crd} }, \code{\link{design.cyclic} }, \code{\link{design.dau} },
\code{\link{design.graeco}}, \code{\link{design.lattice}}, \code{\link{design.split}},
\code{\link{design.rcbd}}, \code{\link{design.strip}} }

\examples{
library(agricolae)
varieties<-c("perricholi","yungay","maria bonita","tomasa")
outdesign <-design.lsd(varieties,serie=2,seed=23)
lsd <- outdesign$book 
print(outdesign$sketch)
print(lsd) # field book.
plots <-as.numeric(lsd[,1])
print(matrix(plots,byrow = TRUE, ncol = 4))
# Write on hard disk.
# write.table(lsd,"lsd.txt", row.names=FALSE, sep="\t")
# file.show("lsd.txt")
}
\keyword{ design }% at least one, from doc/KEYWORDS

