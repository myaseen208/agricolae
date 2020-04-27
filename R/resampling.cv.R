#' Resampling to find the optimal number of markers
#' 
#' This process finds the curve of CV for a different number of markers which
#' allows us to determine the number of optimal markers for a given relative
#' variability.  A method of the curvature.
#' 
#' 
#' @param A data frame or matrix of binary data
#' @param size number of re-samplings
#' @param npoints Number of points to consider the model
#' @return lm(formula = CV ~ I(1/marker))\cr Table with variation coefficient
#' by number of markers
#' @author Felipe de Mendiburu
#' @seealso \code{\link{cv.similarity}}, \code{\link{similarity} }
#' @references Efron, B., Tibshirani, R. (1993) An Introduction to the
#' Boostrap. Chapman and Hall/CRC
#' @keywords optimize
#' @export
#' @examples
#' 
#' library(agricolae)
#' #example table of molecular markers
#' data(markers)
#' study<-resampling.cv(markers,size=1,npoints=15)
#' #
#' # Results of the model
#' summary(study$model)
#' coef<-coef(study$model)
#' py<-predict(study$model)
#' Rsq<-summary(study$model)$"r.squared"
#' table.cv <- data.frame(study$table.cv,estimate=py)
#' print(table.cv)
#' 
#' # Plot CV
#' #startgraph
#' limy<-max(table.cv[,2])+10
#' plot(table.cv[,c(1,2)],col="red",frame=FALSE,xlab="number of markers",
#' ylim=c(10,limy), ylab="CV",cex.main=0.8,main="Estimation of the number of markers")
#' ty<-quantile(table.cv[,2],1)
#' tx<-median(table.cv[,1])
#' tz<-quantile(table.cv[,2],0.95)
#' text(tx,ty, cex=0.8,as.expression(substitute(CV == a + frac(b,markers),
#' list(a=round(coef[1],2),b=round(coef[2],2)))) )
#' text(tx,tz,cex=0.8,as.expression(substitute(R^2==r,list(r=round(Rsq,3)))))
#' 
#' # Plot CV = a + b/n.markers
#' fy<-function(x,a,b) a+b/x
#' x<-seq(2,max(table.cv[,1]),length=50)
#' y <- coef[1] + coef[2]/x
#' lines(x,y,col="blue")
#' #grid(col="brown")
#' rug(table.cv[,1])
#' #endgraph
#' 
resampling.cv <-
function(A,size,npoints) {
nc.A<-ncol(A)
orden<-1:nc.A
y<-trunc(seq(2,(nc.A-1),length=npoints))
marker<- unique(y)
nmarca<-length(marker)
CV<-rep(0,nmarca)
# Control del tiempo inicio y final
inicio<-Sys.time()
i<-0
# Proceso de re-muestreo
for (k in marker) {
cv.m <- 0
for (m in 1:size) {
muestra <-sample(orden,k)
B<-A[,muestra]
cv.m <- cv.m + cv.similarity(B)
}
i<-i+1
CV[i]<- cv.m/size
}
# Final del proceso de re-muestreo
final<-Sys.time()
# Impresion de resultados
time<-final-inicio
cat("\nTime of process ...",time,"\n")
tabla.cv <- data.frame(marker=marker,CV)
# Estimacion de un modelo para CV.
modelo<-lm(CV ~ I(1/marker))
return(list(model=modelo,table.cv=tabla.cv))
}
