\name{design.graeco}
\alias{design.graeco}
%- design.graeco.
\title{ Graeco - latin square design}
\description{
  A graeco - latin square is a KxK pattern that permits the study of k 
  treatments simultaneously with three different blocking variables, each 
  at k levels.
  
  The function is only for squares of the odd numbers and even numbers (4, 8, 10 and 12)
}
\usage{
design.graeco(trt1, trt2, serie = 2, seed = 0, kinds = "Super-Duper",randomization=TRUE)
}

\arguments{
  \item{trt1}{ Treatments }
  \item{trt2}{ Treatments }
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
\references{ 
1. Statistics for Experimenters Design, Innovation, and Discovery
Second Edition. George E. P. Box. Wiley-Interscience. 2005.

2. Experimental design. Cochran and Cox. Second edition. 
Wiley Classics Library Edition published 1992.
}
\author{ Felipe de Mendiburu }

\seealso{\code{\link{design.ab}}, \code{\link{design.alpha}},\code{\link{design.bib}}, 
\code{\link{design.crd} }, \code{\link{design.cyclic} }, \code{\link{design.dau} },
\code{\link{design.split}}, \code{\link{design.lattice}}, \code{\link{design.lsd}},
\code{\link{design.rcbd}}, \code{\link{design.strip}} }

\examples{
library(agricolae)
T1<-c("a","b","c","d")
T2<-c("v","w","x","y")
outdesign <- design.graeco(T1,T2,serie=1)
graeco<-outdesign$book
plots <-as.numeric(graeco[,1])
print(outdesign$sketch)
print(matrix(plots,byrow=TRUE,ncol=4))
# 10 x 10
T1 <- letters[1:10]
T2 <- 1:10
outdesign <-  design.graeco(T1,T2,serie=2)
print(outdesign$sketch)
}
\keyword{ design }

