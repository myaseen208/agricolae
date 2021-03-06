#' Lattice designs
#' 
#' SIMPLE and TRIPLE lattice designs. It randomizes treatments in k x k
#' lattice.
#' 
#' kinds <- c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper",
#' "Mersenne-Twister", "Knuth-TAOCP", "user-supplied", "Knuth-TAOCP-2002",
#' "default" )
#' 
#' @param trt treatments
#' @param r r=2(simple) or r=3(triple) lattice
#' @param serie number plot, 1: 11,12; 2: 101,102; 3: 1001,1002
#' @param seed seed
#' @param kinds method for to randomize
#' @param randomization TRUE or FALSE - randomize
#' @return \item{parameters}{Design parameters} \item{statistics }{Design
#' statistics} \item{sketch}{Design sketch} \item{book}{Fieldbook}
#' @author Felipe de Mendiburu
#' @seealso \code{\link{design.ab}},
#' \code{\link{design.alpha}},\code{\link{design.bib}}, \code{\link{design.crd}
#' }, \code{\link{design.cyclic} }, \code{\link{design.dau} },
#' \code{\link{design.graeco}}, \code{\link{design.split}},
#' \code{\link{design.lsd}}, \code{\link{design.rcbd}},
#' \code{\link{design.strip}}
#' @references FIELD PLOT TECHNIQUE. Erwin L. LeCLERG. 2nd ed., 1962, Burgess
#' Publishing Company, Minnesota
#' @keywords design
#' @importFrom stats runif
#' @export
#' @examples
#' 
#' library(agricolae)
#' # triple lattice
#' trt<-LETTERS[1:9]
#' outdesign<-design.lattice(trt,r=3,serie=2) # triple lattice design ( 9 trt)
#' # simple lattice
#' trt<-1:100
#' outdesign<-design.lattice(trt,r=2,serie=3) # simple lattice design, 10x10 
#' 
design.lattice <-
function(trt,r=3,serie=2,seed=0,kinds="Super-Duper",randomization=TRUE) {
number<-10
if(serie>0) number<-10^serie
ntr<-length(trt)
k<-sqrt(ntr)
if(r==2) type="simple"
if(r==3) type="triple"
if (seed == 0) {
genera<-runif(1)
seed <-.Random.seed[3]
}
set.seed(seed,kinds)
cat("\nLattice design, ", type, " ",k,"x",k,"\n")
E1 <- (ntr - 1) * (2 - 1)/((ntr - 1) * (2 - 1) + 2 * (k-1))
E2 <- (ntr - 1) * (3 - 1)/((ntr - 1) * (3 - 1) + 3 * (k-1))
parameters<-list(design="lattice",type=type,trt=trt,r=r,serie=serie,seed=seed,kinds=kinds)
statistics<-data.frame(treatmens=ntr,blockSize=k,blocks=k)
rownames(statistics)<-"values"
c1<-rep(0,k*k)
dim(c1)<-c(k,k)
c2<-c1
for (a in 1:k) {
for (b in 1:k) {
p<-k*(a-1)+b # primer  cuadro
c1[a,b]<-p
}
}
c2<-t(c1)
# randomiza cada cuadro
nt<-k*k
t<-1:nt
s<-t
if(randomization)s<-sample(t,nt)
for (a in 1:k) {
for (b in 1:k) {
c1[a,b]<-s[c1[a,b]]
}
}
c2 <- t(c1)
nt <- k * k
t <- 1:nt
sqr<-gl(3,k*k)
nb <- as.numeric(gl(k,k))
block<-c(nb,nb+k,nb+2*k)
# tercer cuadro
latino<-as.character(design.lsd(1:k)$book[,4])
Z<-as.numeric(t(c1))
c3<-Z[order(latino)]
dim(c3)<-c(k,k)
c3<-t(c3)
s<-1:k
if(randomization)s<-sample(1:k,k,replace=FALSE)
c1<-c1[s,]
s<-1:k
if(randomization)s<-sample(1:k,k,replace=FALSE)
c2<-c2[s,]
s<-1:k
if(randomization)s<-sample(1:k,k,replace=FALSE)
c3<-c3[s,]
trt1<-c(as.numeric(t(c1)),as.numeric(t(c2)),as.numeric(t(c3)))
Rep<-as.numeric(sqr)
plots <- Rep*number+(1:ntr)
book<-data.frame(plots,r=factor(Rep),block=factor(block),trt=factor(trt[trt1]))
EF<-E2
if(r ==2 ) {
book<-subset(book,as.numeric(book[,2])<3)
EF <-E1
}
cat("\nEfficiency factor\n(E )", EF, "\n\n<<< Book >>>\n")
tr<-as.character(book[,4])
dim(tr)<-c(k,k,r)
if ( r == 2) design<-list(rep1=t(tr[,,1]),rep2=t(tr[,,2]))
if ( r == 3) design<-list(rep1=t(tr[,,1]),rep2=t(tr[,,2]),rep3=t(tr[,,3]))
statistics<-data.frame(statistics,Efficiency=EF)
outdesign<-list(parameters=parameters, statistics=statistics, sketch=design,book=book)
return(outdesign)
}
