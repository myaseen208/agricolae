#' Incomplete Latin Square Design
#' 
#' Such designs are referred to as Youden squares since they were introduced by
#' Youden (1937) after Yates (1936) considered the special case of column equal
#' to number treatment minus 1. "Random" uses the methods of number generation
#' in R. The seed is by set.seed(seed, kinds).
#' 
#' kinds <- c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper",
#' "Mersenne-Twister", "Knuth-TAOCP", "user-supplied", "Knuth-TAOCP-2002",
#' "default" )
#' 
#' @param trt Treatments
#' @param r Replications or number of columns
#' @param serie number plot, 1: 11,12; 2: 101,102; 3: 1001,1002
#' @param seed seed
#' @param kinds method for to randomize
#' @param first TRUE or FALSE - randomize rep 1
#' @param randomization TRUE or FALSE - randomize
#' @return \item{parameters}{Design parameters} \item{sketch}{Design sketch}
#' \item{book}{Fieldbook}
#' @author Felipe de Mendiburu
#' @seealso \code{\link{design.ab}},
#' \code{\link{design.alpha}},\code{\link{design.bib}}, \code{\link{design.crd}
#' }, \code{\link{design.cyclic} }, \code{\link{design.dau} },
#' \code{\link{design.graeco}}, \code{\link{design.lattice}},
#' \code{\link{design.split}}, \code{\link{design.rcbd}},
#' \code{\link{design.strip}}, \code{\link{design.lsd}}
#' @references Design and Analysis of experiment. Hinkelmann, Klaus and
#' Kempthorne, Oscar. Wiley-Interscience. Copyright (2008) by John Wiley and
#' Sons. Inc., Hoboken, new Yersy
#' @keywords design
#' @importFrom stats runif
#' @export
#' @examples
#' 
#' library(agricolae)
#' varieties<-c("perricholi","yungay","maria bonita","tomasa")
#' r<-3
#' outdesign <-design.youden(varieties,r,serie=2,seed=23)
#' youden <- outdesign$book
#' print(outdesign$sketch)
#' plots <-as.numeric(youden[,1])
#' print(matrix(plots,byrow=TRUE,ncol=r))
#' print(youden) # field book.
#' # Write on hard disk.
#' # write.table(youden,"youden.txt", row.names=FALSE, sep="\t")
#' # file.show("youden.txt")
#' 
design.youden <-
function (trt,r,serie=2,seed=0,kinds="Super-Duper",first=TRUE,randomization=TRUE)
{
number<-10
if(serie>0) number<-10^serie
t <- length(trt) # number of treatment
if (seed == 0) {
genera<-runif(1)
seed <-.Random.seed[3]
}
set.seed(seed,kinds)
parameters<-list(design="youden",trt=trt,r=r,serie=serie,seed=seed,kinds=kinds)
a <- matrix(1,nrow=t,ncol=r)
# Generate matrix random
for (j in 1:r){
i<-c(j:t)
if(j>1) i<-c(i,1:(j-1))
a[,j]<-i
}
m<-1:t
if(randomization)m<-sample(1:t,t)
M<-m[a]
a<-trt[M]
dim(a)<-c(t,r)
# Randomize row and column
m<-1:r
if(randomization)m<-sample(1:r,r)
a[,m]-> a
m<-1:t
if(randomization)m<-c(1,sample(2:t,t-1))
a<-a[m,]
if (!first) {
	m<-order(a[1,])
	a<-a[,m]
}
a<-t(a)
trat<-as.vector(a)
columna <- rep(gl(r, 1), t)
fila <- gl(t, r)
fila <- as.character(fila)
fila <- as.numeric(fila)
plots <- fila*number+(1:r)
book <- data.frame(plots, row = as.factor(fila), col = as.factor(columna),
		trat = as.factor(trat))
names(book)[4] <- c(paste(deparse(substitute(trt))))
outdesign<-list(parameters=parameters,sketch=matrix(book[,4], byrow = TRUE, ncol = r),book=book)
return(outdesign)
}
