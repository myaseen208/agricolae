#' Path Analysis
#' 
#' If the cause and effect relationship is well defined, it is possible to
#' represent the whole system of variables in a diagram form known as
#' path-analysis. The function calculates the direct and indirect effects and
#' uses the variables correlation or covariance.
#' 
#' It is necessary first to calculate the correlations.
#' 
#' @param corr.x Matrix of correlations of the independent variables
#' @param corr.y vector of dependent correlations with each one of the
#' independent ones
#' @return Direct and indirect effects and residual Effect^2.
#' @author Felipe de Mendiburu
#' @seealso \code{\link{correlation}}
#' @references Biometrical Methods in Quantitative Genetic Analysis, Singh,
#' Chaudhary. 1979
#' @keywords multivariate
#' @export
#' @examples
#' 
#' # Path analysis. Multivarial Analysis. Anderson. Prentice Hall, pag 616
#' library(agricolae)
#' # Example 1
#' corr.x<- matrix(c(1,0.5,0.5,1),c(2,2))
#' corr.y<- rbind(0.6,0.7)
#' names<-c("X1","X2")
#' dimnames(corr.x)<-list(names,names)
#' dimnames(corr.y)<-list(names,"Y")
#' path.analysis(corr.x,corr.y)
#' # Example 2
#' # data of the progress of the disease related bacterial wilt to the ground
#' # for the component CE Ca K2 Cu
#' data(wilt)
#' data(soil)
#' x<-soil[,c(3,12,14,20)]
#' y<-wilt[,14]
#' cor.y<-correlation(y,x)$correlation
#' cor.x<-correlation(x)$correlation
#' path.analysis(cor.x,cor.y)
#' 
#' 
path.analysis <-
function(corr.x,corr.y) {
if (ncol(corr.y)>1)corr.y<-t(corr.y)
Direct<-solve(corr.x,corr.y)
n<-ncol(corr.x)
Coeff <- corr.x
for ( i in 1:n) {
for ( j in 1:n) {
Coeff[i,j]<-Direct[j]*corr.x[i,j]
}
}
Residual<-1-t(Direct)%*%corr.y
cat("Direct(Diagonal) and indirect effect path coefficients",
    "\n======================================================\n")
print(Coeff)
cat("\nResidual Effect^2 = ",Residual,"\n")
output<-list(Coeff=Coeff,Residual=as.numeric(Residual))
invisible(output)
}
