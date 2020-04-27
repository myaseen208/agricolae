#' Coefficient of the similarity matrix variation
#' 
#' This process consists of finding the coefficient of the distances of
#' similarity of binary tables (1 and 0) as used for scoring molecular marker
#' data for presence and absence of PCR amplification products.
#' 
#' 
#' @param A matrix of binary data
#' @return Returns the coefficient of variation of the similarity model
#' @author Felipe de Mendiburu
#' @seealso \code{\link{similarity}}, \code{\link{resampling.cv} }
#' @keywords multivariate
#' @importFrom stats sd
#' @export
#' @examples
#' 
#' # molecular markers.
#' library(agricolae)
#' data(markers)
#' cv<-cv.similarity(markers)
#' 
#' 
cv.similarity <-
function(A) {
nc<-ncol(A)
nf <- nrow(A)
suma<-rep(0,nf*(nf-1)/2)
ik<-0
for (k1 in 1:(nf-1)) {
for (k2 in (k1+1):nf) {
mm<- na.omit(c(A[k1,] == A[k2,]))
npar<-length(mm)
sii <- sum(mm)
# npares<- min( length(na.omit(A[k1,])),length(na.omit(A[k2,])) )
ik<-ik+1
suma[ik]<- sii/npar # chequear el numero de posibilidades
}
}
cv<-sd(suma,na.rm=TRUE)*100/mean(suma,na.rm=TRUE)
return(cv)
}
