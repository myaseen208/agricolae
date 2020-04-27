#' Split Plot Design
#' 
#' It generates split plot design.  "Random" uses the methods of number
#' generation in R.  The seed is by set.seed(seed, kinds).
#' 
#' kinds <- c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper",
#' "Mersenne-Twister", "Knuth-TAOCP", "user-supplied", "Knuth-TAOCP-2002",
#' "default" )
#' 
#' @param trt1 Treatments in Plots
#' @param trt2 Treatments in Subplots
#' @param r Replications or blocks
#' @param design Experimental design
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
#' \code{\link{design.lsd}}, \code{\link{design.rcbd}},
#' \code{\link{design.strip}}
#' @references Statistical Procedures for Agricultural Research.  Kwanchai A.
#' Gomez, Arturo A. Gomez. John Wiley & Sons, new York, 1984
#' @keywords design
#' @importFrom stats runif
#' @export
#' @examples
#' 
#' library(agricolae)
#' # 4 treatments and 5 blocks in split-plot
#' t1<-c("A","B","C","D")
#' t2<-c(1,2,3)
#' outdesign <-design.split(t1,t2,r=3,serie=2,seed=45,kinds ="Super-Duper")#seed=45
#' book<-outdesign$book# field book
#' # write in hard disk
#' # write.table(book,"book.txt", row.names=FALSE, sep="\t")
#' # file.show("book.txt")
#' 
#' 
design.split <-
function (trt1, trt2,r=NULL, design=c("rcbd","crd","lsd"),serie = 2, seed = 0, kinds = "Super-Duper",
first=TRUE,randomization=TRUE )
{
    n1<-length(trt1)
    n2<-length(trt2)
    if (seed == 0) {
    genera<-runif(1)
    seed <-.Random.seed[3]
    }
    set.seed(seed,kinds)
    design <- match.arg(design)
    number<-10^serie +1
    if (design == "crd") {
        plan<-design.crd(trt1,r,serie, seed, kinds,randomization)
        k<-3
        }
    if (design == "rcbd"){
        plan<-design.rcbd(trt1,r,serie, seed, kinds, first,randomization)
        k<-3
        }
    if (design == "lsd") {
        plan<-design.lsd(trt1,serie, seed, kinds, first,randomization)
        r<-n1
        k<-4
        }
book<-plan$book
parameters<-plan$parameters
names(parameters)[2]<-"trt1"
parameters$applied<-parameters$design
parameters$design<-"split"
parameters$trt2<-trt2
j<-0
B<-list()
for(i in c(1,7,2,8,3:6)){
j<-j+1
B[[j]]<-parameters[[i]]
names(B)[j]<-names(parameters)[i]
}
nplot<-nrow(book)
d<-NULL
if(randomization){
for(i in 1:nplot)d<-rbind(d,sample(trt2,n2))
}
else{
d<-rbind(d,trt2[1:n2])
}
aa<-data.frame(book,trt2=d[,1])
for(j in 2:n2) aa<-rbind(aa,data.frame(book,trt2=d[,j]))
aa<-aa[order(aa[,1]),]
splots<-rep(gl(n2,1),nplot)
book <- data.frame(plots=aa[,1],splots,aa[,-1])
rownames(book)<-1:(nrow(book))
    names(book)[k+1] <- c(paste(deparse(substitute(trt1))))
    names(book)[k+2] <- c(paste(deparse(substitute(trt2))))
outdesign<-list(parameters=B,book=book)
return(outdesign)
}
