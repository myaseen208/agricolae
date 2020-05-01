#' PLOT AMMI
#' 
#' Biplot AMMI.
#' 
#' type=1 produce graphs biplot. type=2 produce graphs triplot, the components
#' are normalizad in scale 0-1.
#' 
#' @param x object AMMI
#' @param first position axis x, 0=Y-dependent, 1=PC1, 2=PC2, 3=PC3
#' @param second position axis y,0=Y-dependent, 1=PC1, 2=PC2, 3=PC3
#' @param third position axis z,0=Y-dependent, 1=PC1, 2=PC2, 3=PC3
#' @param type 1=biplot, 2= triplot
#' @param number TRUE or FALSE names or number genotypes
#' @param gcol genotype color
#' @param ecol environment color
#' @param angle angle from the shaft of the arrow to the edge of the arrow head
#' @param lwd parameter line width in function arrow
#' @param length parameter length in function arrow
#' @param xlab x labels
#' @param ylab y labels
#' @param xlim x limites
#' @param ylim y limites
#' @param \dots other parameters of plot
#' @author Felipe de Mendiburu
#' @seealso \code{\link{AMMI}}
#' @keywords aplot
#' @importFrom graphics plot arrows
#' @export
#' @examples
#' 
#' library(agricolae)
#' data(plrv)
#' model<- with(plrv,AMMI(Locality, Genotype, Rep, Yield))
#' # biplot PC2 vs PC1
#' plot(model)
#' ## plot PC1 vs Yield
#' plot(model,0,1,gcol="blue",ecol="green")
#' ## triplot PC 2,3,4
#' if (requireNamespace("klaR", quietly = TRUE)) {
#' plot(model,first=2,second=3,third=4, type=2,number=TRUE)
#' }
#' 
plot.AMMI <-
function(x,first=1,second=2,third=3,type=1,number=FALSE,gcol=NULL,ecol=NULL,
angle=25,lwd=1.8,length=0.1,xlab=NULL,ylab=NULL,xlim=NULL,ylim=NULL,...)
{
first<-first+2
second<-second+2
third<-third+2
A<-x$biplot
pc<-x$analysis[,1]
n<-length(pc)
namesAxes<-NULL
for(i in 1:n) namesAxes[i]<-paste("PC",i," (",pc[i],")",sep="")
xylabel<-c(names(A)[1:2],namesAxes)
if(is.null(xlab))xlab<-xylabel[first]
if(is.null(ylab))ylab<-xylabel[second]
if(is.null(xlim))xlim<-range(A[,first])
if(is.null(ylim))ylim<-range(A[,second])
if(is.null(gcol))gcol="blue"
if(is.null(ecol))ecol="brown"
gen<-subset(A,A[,1]=="GEN")
env<-subset(A,A[,1]=="ENV")
ngen<-rownames(gen)
if(number)ngen<-1:(nrow(gen))
if(type==1){
plot(A[,first],A[,second],type="p",cex=0,xlab=xlab,ylab=ylab,xlim=xlim,ylim=ylim,...)
text(env[,first],env[,second],rownames(env),col=ecol)
text(gen[,first],gen[,second],ngen,col=gcol)
xmed<-mean(env[,first])
ymed<-mean(env[,second])
arrows(xmed,ymed,env[,first],0.9*env[,second],angle=angle,lwd=lwd,length=length)
abline(v=xmed,h=ymed)
}
if(type==2){
if (requireNamespace("klaR", quietly = TRUE)) {
lugar <- env[,c(first,second,third)]
clones <- gen[,c(first,second,third)]
maxcp <- max(A[, c(first,second,third)])
mincp <- min(A[, c(first,second,third)])
rango <- maxcp - mincp
clones <- (clones - mincp)/rango
nclon <- nrow(clones)
lugar <- (lugar - mincp)/rango
nlugar <- nrow(lugar)
point1 <- cbind(clones[, 1], clones[, 2], clones[, 3])
point2 <- cbind(lugar[, 1], lugar[, 2], lugar[, 3])
point3 <- cbind(c(0.6, 0.6, 0), c(0, 0.8, 0.8), c(0.6, 0, 0.6))
suppressWarnings(warning(klaR::triplot(point1, cex = 0, grid = TRUE,
label = "", center=TRUE,frame=TRUE)))
if (number == TRUE)
suppressWarnings(warning(text(klaR::tritrafo(point1), as.character(1:nclon),
adj = c(0.5, 0), col = "blue", cex = 0.8)))
if (number == FALSE)
suppressWarnings(warning(text(klaR::tritrafo(point1), rownames(clones),
adj = c(0.5, 0), col = "blue", cex = 0.8)))
suppressWarnings(warning(text(klaR::tritrafo(point2), rownames(lugar),
adj = c(0.5, 0), col = "red", cex = 0.8)))
suppressWarnings(warning(text(klaR::tritrafo(point3), xylabel[c(first,second,third)],
adj = c(0.5, 0), cex = 1)))
klaR::trilines(klaR::centerlines(3), lty = 2.5, col = "green", lwd = 2)
for (i in 1:nlugar) {
suppressWarnings(warning(klaR::trilines(c(point2[i, 1], 1/3), c(point2[i, 2],
 1/3), c(point2[i, 3], 1/3),col = "red", lty = 1)))
}
} else {
return("Please install the package klaR to plot triplot")
}
}
}
