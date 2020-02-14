\name{design.bib}
\alias{design.bib}
%- design.bib.
\title{ Randomized Balanced Incomplete Block Designs. BIB}
\description{
  Creates Randomized Balanced Incomplete Block Design.
  "Random" uses the methods of number generation in R.
  The seed is by set.seed(seed, kinds).
}
\usage{
design.bib(trt, k, r=NULL, serie = 2, seed = 0, kinds = "Super-Duper",
maxRep=20,randomization=TRUE)
}

\arguments{
  \item{trt}{ Treatments}
  \item{k}{ size block }
  \item{r}{ Replications }
  \item{serie}{ number plot, 1: 11,12; 2: 101,102; 3: 1001,1002 }
  \item{seed}{ seed }
  \item{kinds}{ method for to randomize }
  \item{maxRep}{ repetition maximum }
  \item{randomization}{ TRUE or FALSE - randomize}
}
\details{
The package AlgDesign is necessary.

if r = NULL, then it calculates the value of r smaller for k defined.
In the case of r = value, then the possible values for "r" is calculated

K is the smallest integer number of treatments and both values are
consistent in design.

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
\references{
1. Experimental design. Cochran and Cox. Second edition.
Wiley Classics Library Edition published 1992

2. Optimal Experimental Design with R.
Dieter Rasch, Jurgen Pilz, Rob Verdooren and Albrecht Gebhardt.
2011 by Taylor and Francis Group, LLC CRC Press is an imprint of Taylor
and Francis Group, an Informa business.

3. Design of Experiments.
Robert O. Kuehl. 2nd ed., Duxbury, 2000.
}

\author{ Felipe de Mendiburu }

\seealso{\code{\link{design.ab}}, \code{\link{design.alpha}},\code{\link{design.split}}, 
\code{\link{design.crd} }, \code{\link{design.cyclic} }, \code{\link{design.dau} },
\code{\link{design.graeco}}, \code{\link{design.lattice}}, \code{\link{design.lsd}},
\code{\link{design.rcbd}}, \code{\link{design.strip}} }

\examples{
library(agricolae)
# 4 treatments and k=3 size block
trt<-c("A","B","C","D")
k<-3
outdesign<-design.bib(trt,k,serie=2,seed =41,kinds ="Super-Duper") # seed = 41
print(outdesign$parameters)
book<-outdesign$book
plots <-as.numeric(book[,1])
matrix(plots,byrow=TRUE,ncol=k)
print(outdesign$sketch)
# write in hard disk
# write.csv(book,"book.csv", row.names=FALSE)
# file.show("book.csv")

}
\keyword{ design }% at least one, from doc/KEYWORDS

