#' Completely Randomized Design
#' 
#' It generates completely a randomized design with equal or different
#' repetition.  "Random" uses the methods of number generation in R.  The seed
#' is by set.seed(seed, kinds).
#' 
#' kinds <- c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper",
#' "Mersenne-Twister", "Knuth-TAOCP", "user-supplied", "Knuth-TAOCP-2002",
#' "default" )
#' 
#' @param trt Treatments
#' @param r Replications
#' @param serie number plot, 1: 11,12; 2: 101,102; 3: 1001,1002
#' @param seed seed
#' @param kinds method for to randomize
#' @param randomization TRUE or FALSE - randomize
#' @return \item{parameters}{Design parameters} \item{book}{Fieldbook}
#' @author Felipe de Mendiburu
#' @seealso \code{\link{design.ab}},
#' \code{\link{design.alpha}},\code{\link{design.bib}},
#' \code{\link{design.split} }, \code{\link{design.cyclic} },
#' \code{\link{design.dau} }, \code{\link{design.graeco}},
#' \code{\link{design.lattice}}, \code{\link{design.lsd}},
#' \code{\link{design.rcbd}}, \code{\link{design.strip}}
#' @references Introduction to Experimental Statistics.  Ching Chun Li.
#' McGraw-Hill Book Company, INC, New. York, 1964
#' @keywords design
#' @importFrom stats runif
#' @export
#' @examples
#' 
#' library(agricolae)
#' trt <-c("CIP-101","CIP-201","CIP-301","CIP-401","CIP-501")
#' r <-c(4,3,5,4,3)
#' # seed = 12543
#' outdesign1 <-design.crd(trt,r,serie=2,2543,"Mersenne-Twister")
#' book1<-outdesign1
#' # no seed
#' outdesign2 <-design.crd(trt,r,serie=3)
#' print(outdesign2$parameters)
#' book2<-outdesign2
#' # write to hard disk
#' # write.table(book1,"crd.txt", row.names=FALSE, sep="\t")
#' # file.show("crd.txt")
#' 
design.crd <-
function(trt,r,serie=2,seed=0,kinds="Super-Duper",randomization=TRUE)
{
number<-0
if(serie>0) number<-10^serie
junto<-data.frame(trt,r)
junto<-junto[order(junto[,1]),]
TR<-as.character(junto[,1])
r<-as.numeric(junto[,2])
y <- rep(TR[1], r[1])
tr <- length(TR)
if (seed == 0) {
genera<-runif(1)
seed <-.Random.seed[3]
}
set.seed(seed,kinds)
parameters<-list(design="crd",trt=trt,r=r,serie=serie,seed=seed,kinds=kinds,randomization)
for (i in 2:tr) y <- c(y, rep(TR[i], r[i]))
trat<-y
if(randomization)trat <- sample(y, length(y), replace = FALSE)
	plots <- number+1:length(trat)
	dca<-data.frame(plots, trat)
	dca[,1]<-as.numeric(dca[,1])
	xx<-dca[order(dca[,2],dca[,1]),]
	r1<-seq(1,r[1])
for (i in 2:length(r)) {
	r1<-c(r1,seq(1,r[i]))
}
yy<-data.frame(plots=xx[,1],r=r1,xx[,2])
book<-yy[order(yy[,1]),]
rownames(book)<-rownames(yy)
names(book)[3]<-c(paste(deparse(substitute(trt))))
outdesign<-list(parameters=parameters,book=book)
return(outdesign)
}
