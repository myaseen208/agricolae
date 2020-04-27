#' Randomized Complete Block Design
#' 
#' It generates Randomized Complete Block Design.  "Random" uses the methods of
#' number generation in R.  The seed is by set.seed(seed, kinds).
#' 
#' kinds <- c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper",
#' "Mersenne-Twister", "Knuth-TAOCP", "user-supplied", "Knuth-TAOCP-2002",
#' "default" )
#' 
#' @param trt Treatments
#' @param r Replications or blocks
#' @param serie number plot, 1: 11,12; 2: 101,102; 3: 1001,1002
#' @param seed seed
#' @param kinds method for to randomize
#' @param first TRUE or FALSE - randomize rep 1
#' @param continue TRUE or FALSE, continuous numbering of plot
#' @param randomization TRUE or FALSE - randomize
#' @return \item{parameters}{Design parameters} \item{sketch}{Design sketch}
#' \item{book}{Fieldbook}
#' @author Felipe de Mendiburu
#' @seealso \code{\link{design.ab}},
#' \code{\link{design.alpha}},\code{\link{design.bib}}, \code{\link{design.crd}
#' }, \code{\link{design.cyclic} }, \code{\link{design.dau} },
#' \code{\link{design.graeco}}, \code{\link{design.lattice}},
#' \code{\link{design.lsd}}, \code{\link{design.split}},
#' \code{\link{design.strip}}
#' @references Introduction to Experimental Statistics.  Ching Chun Li.
#' McGraw-Hill Book Company, INC, New. York, 1964
#' @keywords design
#' @importFrom stats runif
#' @export
#' @examples
#' 
#' library(agricolae)
#' # 5 treatments and 6 blocks
#' trt<-c("A","B","C","D","E")
#' outdesign <-design.rcbd(trt,6,serie=2,986,"Wichmann-Hill") # seed = 986
#' book <-outdesign$book # field book
#' # write in hard disk
#' # write.table(book,"rcbd.txt", row.names=FALSE, sep="\t")
#' # file.show("rcbd.txt")
#' # Plots in field model ZIGZAG
#' fieldbook <- zigzag(outdesign)
#' print(outdesign$sketch)
#' print(matrix(fieldbook[,1],byrow=TRUE,ncol=5))
#' # continuous numbering of plot
#' outdesign <-design.rcbd(trt,6,serie=0,continue=TRUE)
#' head(outdesign$book)
#' 
design.rcbd <-
function (trt, r,serie=2,seed=0,kinds="Super-Duper",first=TRUE,continue=FALSE,randomization=TRUE )
{
number<-10
if(serie>0) number<-10^serie
ntr <- length(trt)
if (seed == 0) {
genera<-runif(1)
seed <-.Random.seed[3]
}
set.seed(seed,kinds)
parameters<-list(design="rcbd",trt=trt,r=r,serie=serie,seed=seed,kinds=kinds,randomization)
mtr <-trt
if(randomization)mtr <- sample(trt, ntr, replace = FALSE)
block <- c(rep(1, ntr))
for (y in 2:r) {
block <- c(block, rep(y, ntr))
if(randomization)mtr <- c(mtr, sample(trt, ntr, replace = FALSE))
}
if(randomization){
if(!first) mtr[1:ntr]<-trt
}
plots <- block*number+(1:ntr)
book <- data.frame(plots, block = as.factor(block), trt = as.factor(mtr))
names(book)[3] <- c(paste(deparse(substitute(trt))))
names(book)[3]<-c(paste(deparse(substitute(trt))))
if(continue){
start0<-10^serie
if(serie==0) start0<-0
book$plots<-start0+1:nrow(book)
}
outdesign<-list(parameters=parameters,sketch=matrix(book[,3], byrow = TRUE, ncol = ntr),book=book)
return(outdesign)
}
