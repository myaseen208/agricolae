#' AMMI index and yield stability
#' 
#' calculate AMMI stability value (ASV) and Yield stability index (YSI).
#' 
#' AMMI stability value (ASV) was calculated using the following formula, as
#' suggested by Purchase (1997)
#' 
#' ASV = sqrt((SSpc1/SSpc2 * PC1i)^2+(PC2i)^2)
#' 
#' YSI = RASV + RY
#' 
#' RASV = rank(ASV) and RY = rank(Y across by environment)
#' 
#' @param model object AMMI
#' @return \item{ASV}{AMMI stability value} \item{YSI}{Yield stability index}
#' \item{rASV}{Rank of AMMI stability value} \item{rYSI}{Rank of yield
#' stability index} \item{means}{average genotype by environment}
#' @author F. de Mendiburu
#' @seealso \code{\link{AMMI}},\code{\link{plot.AMMI}}
#' @references The use of an AMMI model and its parameters to analyse yield
#' stability in multienvironment trials. N. SABAGHNIA, S.H. SABAGHPOUR AND H.
#' DEHGHANI. Journal of Agricultural Science (2008), 146, 571-581. f 2008
#' Cambridge University Press 571 doi:10.1017/S0021859608007831 Printed in the
#' United Kingdom
#' 
#' Parametric analysis to describe genotype x environment interaction and yield
#' stability in winter wheat. PURCHASE, J. L. (1997). Ph.D. Thesis, Department
#' of Agronomy, Faculty of Agriculture of the University of the Free State,
#' Bloemfontein, South Africa.
#' @keywords models
#' @export
#' @examples
#' 
#' library(agricolae)
#' # Index AMMI
#' data(plrv)
#' model<- with(plrv,AMMI(Locality, Genotype, Rep, Yield, console=FALSE))
#' Idx<-index.AMMI(model)
#' names(Idx)
#' # Crops with improved stability according AMMI.
#' print(Idx[order(Idx[,3]),])
#' # Crops with better response and improved stability according AMMI.
#' print(Idx[order(Idx[,4]),])
#' 
index.AMMI <-
function(model)
{
A<-model$biplot[,1:4]
A<-A[A[,1]=="GEN",-c(1,2)]
pc<-model$analysis[1,4]/model$analysis[2,4]
ASV<-apply(A,1,function(x) sqrt((pc*x[1])^2+(x[2])^2))
rk<-rank(ASV)
B<-model$means
W<-tapply.stat(B[,3],B[,2],function(x) mean(x,rm.na=TRUE))
Rx<-rank(-W[,2])
YSI<-rk+Rx
ranking<-data.frame(ASV,YSI,rASV=rk,rYSI=Rx,means=W[,2])
invisible(ranking)
}
