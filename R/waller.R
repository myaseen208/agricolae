#' Computations of Bayesian t-values for multiple comparisons
#' 
#' A Bayes rule for the symmetric multiple comparisons problem.
#' 
#' K-RATIO (K): value specifies the Type 1/Type 2 error seriousness ratio for
#' the Waller-Duncan test. Reasonable values for KRATIO are 50, 100, and 500,
#' which roughly correspond for the two-level case to ALPHA levels of 0.1,
#' 0.05, and 0.01. By default, the procedure uses the default value of 100.
#' 
#' @param K Is the loss ratio between type I and type II error
#' @param q Numerator Degrees of freedom
#' @param f Denominator Degrees of freedom
#' @param Fc F ratio from an analysis of variance
#' @return Waller value for the Waller and Duncan test.
#' @author Felipe de Mendiburu
#' @seealso \code{\link{waller.test}}
#' @references Waller, R. A. and Duncan, D. B. (1969). A Bayes Rule for the
#' Symmetric Multiple Comparison Problem, Journal of the American Statistical
#' Association 64, pages 1484-1504.
#' 
#' Waller, R. A. and Kemp, K. E. (1976) Computations of Bayesian t-Values for
#' Multiple Comparisons, Journal of Statistical Computation and Simulation, 75,
#' pages 169-172.
#' 
#' Principles and procedures of statistics a biometrical approach Steel & Torry
#' & Dickey. Third Edition 1997.
#' @keywords distribution
#' @importFrom stats dt integrate
#' @export
#' @examples
#' 
#' # Table Duncan-Waller K=100, F=1.2 pag 649 Steel & Torry
#' library(agricolae)
#' K<-100
#' Fc<-1.2
#' q<-c(8,10,12,14,16,20,40,100)
#' f<-c(seq(4,20,2),24,30,40,60,120)
#' n<-length(q)
#' m<-length(f)
#' W.D <-rep(0,n*m)
#' dim(W.D)<-c(n,m)
#' for (i in 1:n) {
#' for (j in 1:m) {
#' W.D[i,j]<-waller(K, q[i], f[j], Fc)
#' }}
#' W.D<-round(W.D,2)
#' dimnames(W.D)<-list(q,f)
#' print(W.D)
#' 
#' 
waller <-
function(K,q,f,Fc) {
a0<-1
b0<-20
for (i in 1:50) {
t<-(b0+a0)/2
g<-function(x,q,f,Fc) x^(-(q+3)/2)*(f+q*Fc/x)^(-(f+q-1)/2)
b<-function(x,q,f,Fc) sqrt((f+q)*(x-1)/(f*x+q*Fc) )
h<-function(z,q,f) ((f+q+z^2)/(f+q-1))*dt(z,q+f)+z*pt(z,q+f)
n0 <- function(x) sqrt(x-1)*g(x,q,f,Fc)*h( t*b(x,q,f,Fc),q,f )
d0<- function(x) sqrt(x-1)*g(x,q,f,Fc)*h( -t*b(x,q,f,Fc),q,f )
n1 <- function(x) sqrt(x-1)*g(x,q,f,Fc)*h( a0*b(x,q,f,Fc),q,f )
d1<- function(x) sqrt(x-1)*g(x,q,f,Fc)*h( -a0*b(x,q,f,Fc),q,f )

# Teoria k = int(numerador,1,inf)/int(denominador,1 inf)
#------------------------
IN0<-integrate(n0,1,Inf)$value
ID0<-integrate(d0, 1, Inf)$value
IN1<-integrate(n1,1,Inf)$value
ID1<-integrate(d1, 1, Inf)$value

if( (K-IN0/ID0)*(K-IN1/ID1) <= 0) b0<- t
if( (K-IN0/ID0)*(K-IN1/ID1) > 0 ) a0<- t
if ( abs(b0-a0) <= 5.0e-4 ) break
}
return(round(t,3))
}
