\name{design.youden}
\alias{design.youden}
%- design.Youden.
\title{ Incomplete Latin Square Design }
\description{
  Such designs are referred to as Youden squares since they were introduced
  by Youden (1937) after Yates (1936) considered the special case of column
  equal to number treatment minus 1. "Random" uses the methods of number
  generation in R. The seed is by set.seed(seed, kinds).
}
\usage{
design.youden(trt, r, serie = 2, seed = 0, kinds = "Super-Duper",first=TRUE
,randomization=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{trt}{ Treatments }
  \item{r}{ Replications or number of columns }
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
  \item{sketch}{Design sketch}
  \item{book}{Fieldbook}
}
\references{ Design and Analysis of experiment.
Hinkelmann, Klaus and Kempthorne, Oscar. Wiley-Interscience.
Copyright (2008) by John Wiley and Sons. Inc., Hoboken, new Yersy }
\author{ Felipe de Mendiburu }

\seealso{\code{\link{design.ab}}, \code{\link{design.alpha}},\code{\link{design.bib}}, 
\code{\link{design.crd} }, \code{\link{design.cyclic} }, \code{\link{design.dau} },
\code{\link{design.graeco}}, \code{\link{design.lattice}}, \code{\link{design.split}},
\code{\link{design.rcbd}}, \code{\link{design.strip}}, \code{\link{design.lsd}} }

\examples{
library(agricolae)
varieties<-c("perricholi","yungay","maria bonita","tomasa")
r<-3
outdesign <-design.youden(varieties,r,serie=2,seed=23)
youden <- outdesign$book
print(outdesign$sketch)
plots <-as.numeric(youden[,1])
print(matrix(plots,byrow=TRUE,ncol=r))
print(youden) # field book.
# Write on hard disk.
# write.table(youden,"youden.txt", row.names=FALSE, sep="\t")
# file.show("youden.txt")
}
\keyword{ design }% at least one, from doc/KEYWORDS

