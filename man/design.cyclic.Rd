\name{design.cyclic}
\alias{design.cyclic}

\title{ Cyclic designs }
\description{
  The cyclic design is a incomplete blocks designs, it is generated from 
  a incomplete block initial of the size k, the plan is generated and 
  randomized. The efficient and robust cyclic designs for 6 to 30 treatments, 
  replications <= 10. 
}
\usage{
design.cyclic(trt, k, r, serie = 2, rowcol = FALSE, seed = 0, kinds = "Super-Duper"
,randomization=TRUE)
}
\arguments{
  \item{trt}{ vector treatments }
  \item{k}{ block size}
  \item{r}{ Replications }
  \item{serie}{ number plot, 1: 11,12; 2: 101,102; 3: 1001,1002 }
  \item{rowcol}{ TRUE: row-column design }
  \item{seed}{ init seed random }
  \item{kinds}{ random method }
  \item{randomization}{ TRUE or FALSE - randomize}
}
\details{
  Number o treatment 6 to 30.
  (r) Replication 2 to 10.
  (k) size of block 2 to 10.
  replication = i*k, "i" is value integer.
}
\value{
  \item{parameters}{Design parameters}
  \item{sketch}{Design sketch}
  \item{book}{Fieldbook}
}
\references{ Kuehl, Robert(2000), Design of Experiments. 2nd ed., Duxbury. 
John, J.A. (1981) Efficient Cyclic Design. J. R. Statist. Soc. B, 43, No. 1, pp, 76-80.
}
\author{ Felipe de Mendiburu }
\seealso{\code{\link{design.ab}}, \code{\link{design.alpha}},\code{\link{design.bib}}, 
\code{\link{design.crd} }, \code{\link{design.split} }, \code{\link{design.dau} },
\code{\link{design.graeco}}, \code{\link{design.lattice}}, \code{\link{design.lsd}},
\code{\link{design.rcbd}}, \code{\link{design.strip}} }

\examples{
library(agricolae)
trt<-letters[1:8]
# block size = 2, replication = 6
outdesign1 <- design.cyclic(trt,k=2, r=6,serie=2)
names(outdesign1)
# groups 1,2,3
outdesign1$sketch[[1]]
outdesign1$sketch[[2]]
outdesign1$sketch[[3]]
outdesign1$book
# row-column design
outdesign2<- design.cyclic(trt,k=2, r=6, serie=2, rowcol=TRUE)
outdesign2$sketch
}
\keyword{ design  }
