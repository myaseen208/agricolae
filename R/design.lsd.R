#' Latin Square Design
#' 
#' It generates Latin Square Design.  "Random" uses the methods of number
#' generation in R.  The seed is by set.seed(seed, kinds).
#' 
#' kinds <- c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper",
#' "Mersenne-Twister", "Knuth-TAOCP", "user-supplied", "Knuth-TAOCP-2002",
#' "default" )
#' 
#' @param trt Treatments
#' @param serie number plot, 1: 11,12; 2: 101,102; 3: 1001,1002
#' @param seed seed
#' @param kinds method for to randomize
#' @param first TRUE or FALSE - randomize rep 1
#' @param randomization TRUE or FALSE - randomize
#' @return \item{parameters}{Design parameters} \item{book}{Fieldbook}
#' @author Felipe de Mendiburu
#' @seealso \code{\link{design.ab}},
#' \code{\link{design.alpha}},\code{\link{design.bib}}, \code{\link{design.crd}
#' }, \code{\link{design.cyclic} }, \code{\link{design.dau} },
#' \code{\link{design.graeco}}, \code{\link{design.lattice}},
#' \code{\link{design.split}}, \code{\link{design.rcbd}},
#' \code{\link{design.strip}}
#' @references Introduction to Experimental Statistics.  Ching Chun Li.
#' McGraw-Hill Book Company, INC, New. York, 1969
#' @keywords design
#' @importFrom stats runif
#' @export
#' @examples
#' 
#' library(agricolae)
#' varieties<-c("perricholi","yungay","maria bonita","tomasa")
#' outdesign <-design.lsd(varieties,serie=2,seed=23)
#' lsd <- outdesign$book 
#' print(outdesign$sketch)
#' print(lsd) # field book.
#' plots <-as.numeric(lsd[,1])
#' print(matrix(plots,byrow = TRUE, ncol = 4))
#' # Write on hard disk.
#' # write.table(lsd,"lsd.txt", row.names=FALSE, sep="\t")
#' # file.show("lsd.txt")
#' 
design.lsd <-
function (trt,serie=2,seed=0,kinds="Super-Duper",first=TRUE,randomization=TRUE)
{
number<-10
if(serie>0) number<-10^serie
r <- length(trt)
if (seed == 0) {
genera<-runif(1)
seed <-.Random.seed[3]
}
set.seed(seed,kinds)
parameters<-list(design="lsd",trt=trt,r=r,serie=serie,seed=seed,kinds=kinds,randomization)
a <- 1:(r * r)
dim(a) <- c(r, r)
for (i in 1:r) {
for (j in 1:r) {
k <- i + j - 1
if (k > r)
k <- i + j - r - 1
a[i, j] <- k
}
}
m<-2:r
if(randomization)m<-sample(2:r,r-1)
a<-a[,c(1,m)]
if(randomization){
if (first) {
	m<-sample(1:r,r)
	a<-a[m,]
}}
trat<-trt[a]
columna <- rep(gl(r, 1), r)
fila <- gl(r, r)
fila <- as.character(fila)
fila <- as.numeric(fila)
plots <- fila*number+(1:r)
book <- data.frame(plots, row = as.factor(fila), col = as.factor(columna),
		trat = as.factor(trat))
names(book)[4] <- c(paste(deparse(substitute(trt))))
outdesign<-list(parameters=parameters,sketch=matrix(book[,4], byrow = TRUE, ncol = r),book=book)
return(outdesign)
}
