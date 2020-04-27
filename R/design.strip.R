#' Strip Plot Design
#' 
#' It generates strip plot design.  "Random" uses the methods of number
#' generation in R.  The seed is by set.seed(seed, kinds).
#' 
#' kinds <- c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper",
#' "Mersenne-Twister", "Knuth-TAOCP", "user-supplied", "Knuth-TAOCP-2002",
#' "default" )
#' 
#' @param trt1 Row treatments
#' @param trt2 column treatments
#' @param r Replications
#' @param serie number plot, 1: 11,12; 2: 101,102; 3: 1001,1002
#' @param seed seed
#' @param kinds method for to randomize
#' @param randomization TRUE or FALSE - randomize
#' @return \item{parameters}{Design parameters} \item{book}{Fieldbook}
#' @author Felipe de Mendiburu
#' @seealso \code{\link{design.ab}},
#' \code{\link{design.alpha}},\code{\link{design.bib}}, \code{\link{design.crd}
#' }, \code{\link{design.cyclic} }, \code{\link{design.dau} },
#' \code{\link{design.graeco}}, \code{\link{design.lattice}},
#' \code{\link{design.lsd}}, \code{\link{design.rcbd}},
#' \code{\link{design.split}}
#' @references Statistical Procedures for Agricultural Research.  Kwanchai A.
#' Gomez, Arturo A. Gomez. John Wiley & Sons, new York, 1984
#' @keywords design
#' @importFrom stats runif
#' @export
#' @examples
#' 
#' library(agricolae)
#' # 4 and 3 treatments and 3 blocks in strip-plot
#' t1<-c("A","B","C","D")
#' t2<-c(1,2,3)
#' r<-3
#' outdesign <-design.strip(t1,t2,r, serie=2,seed=45,kinds ="Super-Duper") # seed = 45
#' book <-outdesign$book # field book
#' # write in hard disk
#' # write.table(book,"book.txt", row.names=FALSE, sep="\t")
#' # file.show("book.txt")
#' 
#' 
design.strip <-
function (trt1, trt2,r, serie = 2, seed = 0, kinds = "Super-Duper",randomization=TRUE)
{
number<-10
if(serie>0) number<-10^serie
    n1<-length(trt1)
    n2<-length(trt2)
    if (seed == 0) {
    genera<-runif(1)
    seed <-.Random.seed[3]
    }
        set.seed(seed, kinds)
a<-trt1[1:n1]
b<-trt2[1:n2]
if(randomization){
        a<-sample(trt1,n1)
        b<-sample(trt2,n2)
}
        fila<-rep(b,n1)
        columna <- a[gl(n1,n2)]
        block <- rep(1,n1*n2)
    if (r > 1) {
    for (i in 2:r) {
a<-trt1[1:n1]
b<-trt2[1:n2]
if(randomization){
        a<-sample(trt1,n1)
        b<-sample(trt2,n2)
}
        fila<-c(fila,rep(b,n1))
        columna <- c(columna,a[gl(n1,n2)])
        block <- c(block,rep(i,n1*n2))
    }}
    parameters<-list(design="strip",trt1=trt1,trt2=trt2,r=r,serie=serie,seed=seed,kinds=kinds)
    plots <- block*number+1:(n1*n2)
    book <- data.frame(plots, block = as.factor(block), column=as.factor(columna),row = as.factor(fila))
    names(book)[3] <- c(paste(deparse(substitute(trt1))))
    names(book)[4] <- c(paste(deparse(substitute(trt2))))
    outdesign<-list(parameters=parameters,book=book)
    return(outdesign)
}
