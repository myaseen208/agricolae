#' Design of experiments for a factorial
#' 
#' It generates a design of blocks, randomize and latin square for combined n.
#' factors uses the methods of number generation in R. The seed is by
#' set.seed(seed, kinds).
#' 
#' kinds <- c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper",
#' "Mersenne-Twister", "Knuth-TAOCP", "user-supplied", "Knuth-TAOCP-2002",
#' "default" )
#' 
#' @param trt n levels factors
#' @param r Replications or Blocks
#' @param serie number plot, 1: 11,12; 2: 101,102; 3: 1001,1002
#' @param design type
#' @param seed Seed
#' @param kinds Method for to randomize
#' @param first TRUE or FALSE - randomize rep 1
#' @param randomization TRUE or FALSE - randomize
#' @return \item{parameters}{Design parameters} \item{book}{Fieldbook}
#' @author Felipe de Mendiburu
#' @seealso \code{\link{design.split}},
#' \code{\link{design.alpha}},\code{\link{design.bib}}, \code{\link{design.crd}
#' }, \code{\link{design.cyclic} }, \code{\link{design.dau} },
#' \code{\link{design.graeco}}, \code{\link{design.lattice}},
#' \code{\link{design.lsd}}, \code{\link{design.rcbd}},
#' \code{\link{design.strip}}
#' @references Introduction to Experimental Statistics.  Ching Chun Li.
#' McGraw-Hill Book Company, INC, New. York, 1964
#' @keywords design
#' @export
#' @examples
#' 
#' # factorial 3 x 2 with 3 blocks
#' library(agricolae)
#' trt<-c(3,2) # factorial 3x2
#' outdesign <-design.ab(trt, r=3, serie=2)
#' book<-outdesign$book
#' head(book,10) # print of the field book
#' # factorial 2 x 2 x 2 with 5 replications in completely randomized design.
#' trt<-c(2,2,2)
#' outdesign<-design.ab(trt, r=5, serie=2,design="crd")
#' book<-outdesign$book
#' print(book)
#' # factorial 3 x 3 in latin square design.
#' trt <-c(3,3)
#' outdesign<-design.ab(trt, serie=2, design="lsd")
#' book<-outdesign$book
#' print(book)
#' 
design.ab <-
function(trt, r=NULL,serie=2,design=c("rcbd","crd","lsd"),seed=0,kinds="Super-Duper",
first=TRUE,randomization=TRUE ){
design <- match.arg(design)
if( design=="rcbd" | design=="crd") posicion <- 3
else posicion <- 4
serie<-serie; seed<-seed; kinds<-kinds; first<-first;

# Process to trt to factorial
ntr<-length(trt)
fact<-NULL
tr0<-1:trt[1]
k<-0
a<-trt[1];b<-trt[2]
for(i in 1:a){
for(j in 1:b){
k<-k+1
fact[k]<-paste(tr0[i],j)
}
}

if(ntr >2) {
for(m in 3:ntr){
k<-0
tr0<-fact
fact<-NULL
a<-a*b
b<-trt[m]
for(i in 1:a){
for(j in 1:b){
k<-k+1
fact[k]<-paste(tr0[i],j)
}
}
}
}
#------------------------------
if(design=="rcbd")plan<-design.rcbd(trt=fact, r, serie, seed, kinds, first,randomization )
if(design=="crd")plan<-design.crd(trt=fact, r, serie, seed, kinds,randomization)
if(design=="lsd")plan<-design.lsd(trt=fact, serie, seed, kinds, first,randomization )
parameters<-plan$parameters
parameters$applied<-parameters$design
parameters$design<-"factorial"
plan<-plan$book
trt<-as.character(plan[,posicion])
nplan<-nrow(plan)
A<-rep(" ",nplan*ntr)
dim(A)<-c(nplan,ntr)
colnames(A)<-LETTERS[1:ntr]

for(i in 1:nplan) {
A[i,]<-unlist(strsplit(trt[i], " "))
}
A<-as.data.frame(A)
book<-data.frame(plan[,1:(posicion-1)],A)
outdesign<-list(parameters=parameters,book=book)
return(outdesign)
}
