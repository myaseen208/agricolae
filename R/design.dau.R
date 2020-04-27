#' Augmented block design
#' 
#' These are designs for two types of treatments: the control treatments
#' (common) and the increased treatments. The common treatments are applied in
#' complete randomized blocks, and the increased treatments, at random. Each
#' treatment should be applied in any block once only. It is understood that
#' the common treatments are of a greater interest; the standard error of the
#' difference is much smaller than when between two increased ones in different
#' blocks.
#' 
#' kinds <- c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper",
#' "Mersenne-Twister", "Knuth-TAOCP", "user-supplied", "Knuth-TAOCP-2002",
#' "default" )
#' 
#' @param trt1 checks
#' @param trt2 new
#' @param r Replications or blocks
#' @param serie number plot, 1: 11,12; 2: 101,102; 3: 1001,1002
#' @param seed seed
#' @param kinds method for to randomize
#' @param name name of treatments
#' @param randomization TRUE or FALSE - randomize
#' @return \item{parameters}{Design parameters} \item{book}{Fieldbook}
#' @author Felipe de Mendiburu
#' @seealso \code{\link{design.ab}},
#' \code{\link{design.alpha}},\code{\link{design.bib}}, \code{\link{design.crd}
#' }, \code{\link{design.cyclic} }, \code{\link{design.split} },
#' \code{\link{design.graeco}}, \code{\link{design.lattice}},
#' \code{\link{design.lsd}}, \code{\link{design.rcbd}},
#' \code{\link{design.strip}}
#' @references 1. Augmented (or Hoonuiaku) Design. Federer, W.T. (1956), Hawaii
#' Plr. rec., 55: 191-208. 2. In Augmented Designs. Federer, W.T and
#' Raghavarao, D. (1975). Bometrics, vol. 31, No. 1 (mar.., 1975), pp. 29-35
#' @keywords design
#' @importFrom stats runif
#' @export
#' @examples
#' 
#' library(agricolae)
#' # 4 treatments and 5 blocks
#' T1<-c("A","B","C","D")
#' T2<-letters[20:26]
#' outdesign <-design.dau(T1,T2, r=5,serie=2)
#' # field book
#' book<-outdesign$book
#' by(book,book[2],function(x) paste(x[,1],"-",as.character(x[,3])))
#' # write in hard disk
#' # write.table(book,"dau.txt", row.names=FALSE, sep="\t")
#' # file.show("dau.txt")
#' # Augmented designs in Completely Randomized Design
#' trt<-c(T1,T2)
#' r<-c(4,4,4,4,1,1,1,1,1,1,1)
#' outdesign <- design.crd(trt,r)
#' outdesign$book
#' 
design.dau <-
function (trt1, trt2, r,serie=2,seed=0,kinds="Super-Duper",name="trt",randomization=TRUE)
{
number<-10
if(serie>0) number<-10^serie
ntr1 <- length(trt1)
if (seed == 0) {
genera<-runif(1)
seed <-.Random.seed[3]
}
set.seed(seed,kinds)
parameters<-list(design="dau",trt1=trt1,trt2=trt2,r=r,serie=serie,seed=seed,kinds=kinds)
mtr1 <- trt1
if(randomization)mtr1 <- sample(trt1, ntr1, replace = FALSE)
block <- c(rep(1, ntr1))
for (y in 2:r) {
block <- c(block, rep(y, ntr1))
mtr1 <- mtr1
if(randomization)mtr1 <- c(mtr1, sample(trt1, ntr1, replace = FALSE))
}
ntr2 <- length(trt2)
mtr2 <- trt2
if(randomization)mtr2 <- sample(trt2,ntr2, replace = FALSE)
s<-s<-1:ntr2%%r
for(i in 1:ntr2) if(s[i]==0)s[i]<-r
block <- c(block, s)
mtr <- c(mtr1,mtr2)
nr<-as.numeric(table(block))
nt<-length(nr)
plots<-NULL
for(i in 1:nt) plots<-c(plots,i*number+1:nr[i])
book<-data.frame(block=as.factor(block),trt=as.factor(mtr))
book<-book[order(book[,1]),]
for (i in 1:r)
if(randomization)book[book[,1]==i,2]<-sample(book[book[,1]==i,2],length(book[book[,1]==i,2]))
book<-data.frame(plots,book)
rownames(book)=1:nrow(book)
names(book)[3]<-name
outdesign<-list(parameters=parameters,book=book)
return(outdesign)
}
