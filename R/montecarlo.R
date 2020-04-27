#' Random generation by Montecarlo
#' 
#' Random generation form data, use function density and parameters
#' 
#' 
#' @param data vector or object(hist, graph.freq)
#' @param k number of simulations
#' @param \dots Other parameters of the function density, only if data is
#' vector
#' @return Generate random numbers with empirical distribution.
#' @author Felipe de Mendiburu
#' @seealso \code{\link{density}}
#' @keywords manip
#' @importFrom stats density
#' @export
#' @examples
#' 
#' library(agricolae)
#' r<-rnorm(50, 10,2)
#' montecarlo(r, k=100, kernel="epanechnikov")
#' # other example
#' h<-hist(r,plot=FALSE)
#' montecarlo(h, k=100)
#' # other example
#' breaks<-c(0, 150, 200, 250, 300)
#' counts<-c(10, 20, 40, 30)
#' par(mfrow=c(1,2),cex=0.8,mar=c(2,3,0,0))
#' h1<-graph.freq(x=breaks,counts=counts,plot=FALSE)
#' r<-montecarlo(h, k=1000)
#' plot(h1,frequency = 3,ylim=c(0,0.008))
#' text(90,0.006,"Population\n100 obs.")
#' h2<-graph.freq(r,breaks,frequency = 3,ylim=c(0,0.008))
#' lines(density(r),col="blue")
#' text(90,0.006,"Montecarlo\n1000 obs.")
#' 
#' 
montecarlo <-
function(data, k, ...) {
if (is.list(data)) {
f <- data$counts
breaks <- data$breaks
tot <- sum(f)
n <- length(data$mids)
breaks <- c(breaks, 2 * breaks[n + 1] - breaks[n])
y <- rep(0, n + 1)
for (i in 1:n) {
y[i + 1] <- y[i] + f[i]/tot
}
y <- c(y, 1)
ns<-length(breaks)
sk <- runif(k)
w<-rep(0,k)
for (j in 1:k) {
for (i in 1:(ns-1))  {
if ( (sk[j] >= y[i]) & (sk[j] <= y[i+1] ) ){
w[j] <- (breaks[i]*(y[i+1]-sk[j])+breaks[i+1]*(sk[j]-y[i]))/(y[i+1] -y[i])
break
} 
} 
}
}
else
{
s<-density(data, ...)
x<-s$x
ancho<- s$bw
ns<-length(x)
y<-s$y
# areas
A<-rep(0,(ns-1))
for(i in 1:(ns-1)) A[i] <- (y[i] + y[i+1])*ancho/2
total <- sum(A)
pa<-rep(0,(ns-1))
pa[1] <- A[1]
for(i in 2:(ns-1)) pa[i] <- pa[i-1]+A[i]
pa<-pa/total
x<- x[-1]
# Genera runif(k)
sk <- runif(k)
w<-data.frame(orden=1:k, sk, aleatorio=0)
w<-w[order(w[,2]),]
start <- 1
for (i in 1:k) {
for (j in start:ns)  {
if ( w[i,2] < pa[j] ) {
w[i,3] <- x[j]
start <- j
break
} 
} 
}
w<-w[order(w[,1]),3]
}
return(w)
}
