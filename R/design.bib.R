#' Randomized Balanced Incomplete Block Designs. BIB
#' 
#' Creates Randomized Balanced Incomplete Block Design.  "Random" uses the
#' methods of number generation in R.  The seed is by set.seed(seed, kinds).
#' 
#' The package AlgDesign is necessary.
#' 
#' if r = NULL, then it calculates the value of r smaller for k defined. In the
#' case of r = value, then the possible values for "r" is calculated
#' 
#' K is the smallest integer number of treatments and both values are
#' consistent in design.
#' 
#' kinds <- c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper",
#' "Mersenne-Twister", "Knuth-TAOCP", "user-supplied", "Knuth-TAOCP-2002",
#' "default" )
#' 
#' @param trt Treatments
#' @param k size block
#' @param r Replications
#' @param serie number plot, 1: 11,12; 2: 101,102; 3: 1001,1002
#' @param seed seed
#' @param kinds method for to randomize
#' @param maxRep repetition maximum
#' @param randomization TRUE or FALSE - randomize
#' @return \item{parameters}{Design parameters} \item{statistics }{Design
#' statistics} \item{sketch}{Design sketch} \item{book}{Fieldbook}
#' @author Felipe de Mendiburu
#' @seealso \code{\link{design.ab}},
#' \code{\link{design.alpha}},\code{\link{design.split}},
#' \code{\link{design.crd} }, \code{\link{design.cyclic} },
#' \code{\link{design.dau} }, \code{\link{design.graeco}},
#' \code{\link{design.lattice}}, \code{\link{design.lsd}},
#' \code{\link{design.rcbd}}, \code{\link{design.strip}}
#' @references 1. Experimental design. Cochran and Cox. Second edition. Wiley
#' Classics Library Edition published 1992
#' 
#' 2. Optimal Experimental Design with R. Dieter Rasch, Jurgen Pilz, Rob
#' Verdooren and Albrecht Gebhardt. 2011 by Taylor and Francis Group, LLC CRC
#' Press is an imprint of Taylor and Francis Group, an Informa business.
#' 
#' 3. Design of Experiments. Robert O. Kuehl. 2nd ed., Duxbury, 2000.
#' @keywords design
#' @importFrom stats runif
#' @export
#' @examples
#' 
#' library(agricolae)
#' # 4 treatments and k=3 size block
#' trt<-c("A","B","C","D")
#' k<-3
#' outdesign<-design.bib(trt,k,serie=2,seed =41,kinds ="Super-Duper") # seed = 41
#' print(outdesign$parameters)
#' book<-outdesign$book
#' plots <-as.numeric(book[,1])
#' matrix(plots,byrow=TRUE,ncol=k)
#' print(outdesign$sketch)
#' # write in hard disk
#' # write.csv(book,"book.csv", row.names=FALSE)
#' # file.show("book.csv")
#' 
#' 
design.bib <-
function (trt, k, r=NULL, serie = 2, seed = 0, kinds = "Super-Duper", maxRep=20,randomization=TRUE)
{
v<-length(trt)
number<-10
if(serie>0) number<-10^serie
    ntr <- length(trt)
    if (seed == 0) {
    genera<-runif(1)
    seed <-.Random.seed[3]
    }
    set.seed(seed, kinds)
#----------
# question r
if(!is.null(r)){
x<- r*(k-1)/(v-1)
b<- v*r/k
y<- ceiling(x)
z<- ceiling(b)
if(!(x==y & b==z)) {
# change r
r<-NULL
for (i in 2:maxRep){
x<- i*(k-1)/(v-1)
b<- v*i/k
y<- ceiling(x)
z<- ceiling(b)
if(x==y & b==z)r<-c(r,i)
}
if(!is.null(r) ) return(cat("\n Change r by ",paste(r, collapse = ", "),"...\n"))
else return(cat("Other k <> ",k,"; 1<k<",v,"\n"))
}
}

if(is.null(r)){
for (i in 2:maxRep){
x<- i*(k-1)/(v-1)
b<- v*i/k
y<- ceiling(x)
z<- ceiling(b)
if(x==y & b==z)r<-c(r,i)
}
r<-r[1]
}
b<- v*r/k
if (requireNamespace("AlgDesign", quietly = TRUE)) {
initial <- AlgDesign::optBlock(~., withinData = factor(1:v), blocksizes = rep(k,b))$row
md <- matrix(initial, byrow = TRUE, ncol = k)
}
#----------
    b<-nrow(md)
    bp<-1:b
    if(randomization)bp<-sample(1:b,b)
    md<- md[bp,]
    for (i in 1:b) {
    bi<-1:k
    if(randomization)bi<-sample(1:k,k)
    md[i,]<- md[i,bi]
    }
mtr<-trt[t(md)]
block <- gl(b,k)
Rep<-as.numeric(block)
plots <- Rep*number+(1:k)
parameters<-list(design="bib",trt=trt,k=k,serie=serie,seed=seed,kinds=kinds)
#plots <- number + 1:(b*k) - 1
book <- data.frame(plots, block = as.factor(block), trt = as.factor(mtr))
names(book)[3] <- c(paste(deparse(substitute(trt))))
r<-as.numeric(table(book[,3])[1])
lambda<-r*(k-1)/(ntr-1)
E<-lambda*ntr/(r*k)
cat("\nParameters BIB\n==============")
cat("\nLambda     :",lambda)
cat("\ntreatmeans :",ntr)
cat("\nBlock size :",k)
cat("\nBlocks     :",b)
cat("\nReplication:",r,"\n")
cat("\nEfficiency factor",E,"\n\n<<< Book >>>\n")
statistics<-data.frame(lambda= lambda,treatmeans=ntr,blockSize=k,blocks=b,r=r,Efficiency=E)
rownames(statistics)<-"values"
outdesign<-list(parameters=parameters,statistics=statistics,
sketch=matrix(book[,3], byrow = TRUE, ncol = k),book=book)
return(outdesign)
}
