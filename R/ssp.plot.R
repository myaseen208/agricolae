#' Split-split-Plot analysis
#' 
#' The variance analysis of a split-split plot design is divided into three
#' parts: the main-plot, subplot and sub-subplot analysis.
#' 
#' The split-split-plot design is an extension of the split-plot design to
#' accommodate a third factor: one factor in main-plot, other in subplot and
#' the third factor in sub-subplot
#' 
#' @param block replications
#' @param pplot Factor main plot
#' @param splot Factor subplot
#' @param ssplot Factor sub-subplot
#' @param Y Variable, response
#' @return ANOVA: Splip Split plot analysis
#' @author Felipe de Mendiburu
#' @seealso \code{\link{sp.plot}}, \code{\link{strip.plot}},
#' \code{\link{design.split}}, \code{\link{design.strip} }
#' @references Statistical procedures for agricultural research.  Kwanchai A.
#' Gomez, Arturo A. Gomez. Second Edition. 1984.
#' @keywords models
#' @export
#' @examples
#' 
#' # Statistical procedures for agricultural research, pag 143
#' # Grain Yields of Three Rice Varieties Grown under 
#' #Three Management practices and Five Nitrogen levels; in a
#' #split-split-plot design with nitrogen as main-plot, 
#' #management practice as subplot, and variety as sub-subplot 
#' #factores, with three replications.
#' library(agricolae)
#' f <- system.file("external/ssp.csv", package="agricolae")
#' ssp<-read.csv(f)
#' model<-with(ssp,ssp.plot(block,nitrogen,management,variety,yield))
#' gla<-model$gl.a; glb<-model$gl.b; glc<-model$gl.c
#' Ea<-model$Ea; Eb<-model$Eb; Ec<-model$Ec
#' par(mfrow=c(1,3),cex=0.6)
#' out1<-with(ssp,LSD.test(yield,nitrogen,gla,Ea,console=TRUE))
#' out2<-with(ssp,LSD.test(yield,management,glb,Eb,console=TRUE))
#' out3<-with(ssp,LSD.test(yield,variety,glc,Ec,console=TRUE))
#' plot(out1,xlab="Nitrogen",las=1,variation="IQR")
#' plot(out2,xlab="Management",variation="IQR")
#' plot(out3,xlab="Variety",variation="IQR")
#' # with aov
#' AOV<-aov(yield ~  block + nitrogen*management*variety + Error(block/nitrogen/management),data=ssp)
#' summary(AOV)
#' 
ssp.plot <-
function(block, pplot, splot, ssplot, Y)
{
name.y <- paste(deparse(substitute(Y)))
name.r <- paste(deparse(substitute(block)))
name.p <- paste(deparse(substitute(pplot)))
name.sp <- paste(deparse(substitute(splot)))
name.ssp <- paste(deparse(substitute(ssplot)))
block<-as.factor(block)
pplot<-as.factor(pplot)
splot<-as.factor(splot)
ssplot<-as.factor(ssplot)
cat("\nANALYSIS SPLIT-SPLIT PLOT: ", name.y, "\nClass level information\n\n")
nrep <- length(unique(block))
np  <- length(unique(pplot))
nsp <- length(unique(splot))
nssp<- length(unique(ssplot))
cat(name.p,  "\t: ",unique(as.character(pplot)),"\n")
cat(name.sp, "\t: ",unique(as.character(splot)),"\n")
cat(name.ssp,"\t: ",unique(as.character(ssplot)),"\n")
cat(name.r,  "\t: ",unique(as.character(block)),"\n")
cat("\nNumber of observations: ", length(Y), "\n\n")

model<- aov(Y ~ block*pplot*splot*ssplot)
B<-suppressWarnings(anova(model))
W<-NULL
W<-B[c(1,2,16,3,7,16,4,9,10,14,16),]
for (j in 1:2){
W[3,j]<-B[5,j]
W[6,j]<-B[6,j]+B[11,j]
W[11,j]<-B[8,j]+B[12,j]+B[13,j]+B[15,j]
}
W[,3]<-W[,2]/W[,1]
W[1:2,4]<-W[1:2,3]/W[3,3]
W[4:5,4]<-W[4:5,3]/W[6,3]
W[7:10,4]<-W[7:10,3]/W[11,3]
# Pvalue
W[1:2,5]<-1-pf(W[1:2,4],W[1:2,1],W[3,1])
W[4:5,5]<-1-pf(W[4:5,4],W[4:5,1],W[6,1])
W[7:10,5]<-1-pf(W[7:10,4],W[7:10,1],W[11,1])
#W[,5]<-round(W[,5],4)
N<-NULL
N[1]<- name.r
N[2]<- name.p
N[3]<- "Ea"
N[4]<- name.sp
N[5]<- paste(name.p,":",name.sp,sep="")
N[6]<- "Eb"
N[7]<- name.ssp
N[8]<- paste(name.ssp,":",name.p,sep="")
N[9]<- paste(name.ssp,":",name.sp,sep="")
N[10]<-paste(name.ssp,":",name.p,":",name.sp,sep="")
N[11]<- "Ec"
rownames(W)<-N
attributes(W)$heading[2]<-paste("Response:",name.y)
print(W)
medy <- mean(Y,na.rm=TRUE)
gl.a<-W[3,1]; Ea<-W[3,3]
gl.b<-W[6,1]; Eb<-W[6,3]
gl.c<-W[11,1]; Ec<-W[11,3]
cat("\ncv(a) =",round(sqrt(Ea)*100/medy,1),"%,", 
"cv(b) =",round(sqrt(Eb)*100/medy,1),"%,",
"cv(c) =",round(sqrt(Ec)*100/medy,1),"%,","Mean =", medy,"\n\n")
output<-list(ANOVA=W, gl.a=gl.a,gl.b=gl.b,gl.c=gl.c,Ea=Ea,Eb=Eb,Ec=Ec)
invisible(output)
}
