#' Uniformity soil. Smith's Index of Soil Heterogeneity
#' 
#' Smith's index of soil heterogeneity is used primarily to derive optimum plot
#' size. The index gives a single value as a quantitative measure of soil
#' heterogeneity in an area.  Graph CV for plot size and shape.
#' 
#' 
#' Vx=V(x)/x b
#' 
#' V(x) is the between-plot variance, Vx is the variance per unit area for plot
#' size of x basic unit, and b is the Smith' index of soil heterogeneity.
#' 
#' @param data dataframe or matrix
#' @param PLOT graphics, TRUE or FALSE
#' @param \dots Parameters of the plot()
#' @return \item{model}{function pattern of uniformity} \item{uniformity}{Table
#' of the soil uniformity}
#' @author Felipe de Mendiburu
#' @references Statistical Procedures for Agriculture Research. Second Edition.
#' Kwanchai A. Gomez and Arturo A. Gomez. 1976. USA
#' @keywords design
#' @importFrom stats coef
#' @importFrom graphics plot
#' @export
#' @examples
#' 
#' library(agricolae)
#' data(rice)
#' #startgraph
#' table<-index.smith(rice,
#'  main="Relationship between CV per unit area and plot size",col="red")
#' #endgraph
#' uniformity <- data.frame(table$uniformity)
#' uniformity
#' # regression variance per unit area an plot size.
#' model <- lm(Vx ~ I(log(Size)),uniformity)
#' coeff <- coef(model)
#' x<-1:max(uniformity$Size)
#' Vx<- coeff[1]+coeff[2]*log(x)
#' #startgraph
#' plot(x,Vx, type="l", col="blue",
#'  main="Relationship between variance per unit area and plot size")
#' points(uniformity$Size,uniformity$Vx)
#' #endgraph
#' 
index.smith <-
function(data,PLOT=TRUE,...) {
A<-as.matrix(data)
n<-nrow(A)
m<-ncol(A)
TC <- sum(A)^2/(n*m)
media <- mean(A)
m1<-1:m
n1<-1:n
k0<-m1[m/m1==m%/%m1] # para las columnas
r0<-n1[n/n1==n%/%n1] # para las filas
lk0 <-length(k0)
lr0 <-length(r0)
k0 <- k0[-lk0]
r0 <- r0[-lr0]
lk0 <-length(k0)
lr0 <-length(r0)
x<-rep(0,lk0*lr0);V<-x; CV<-x;Width<-x;Length<-x;l<-0
for (k in k0) {
for (p in r0) {
l<-l+1
ss<-0
k3<-0;k4<-0
for (i in 1:(n/p)) {
k3<- k4+1
k4<- k3+p-1
k1<-0;k2<-0
for (j in 1: (m/k)) {
k1<- k2+1
k2 <-k1+k-1
ss<-ss+(sum(A[k3:k4,k1:k2]))^2/(k*p)
}
}

V[l]<-ss-TC
#V[l]<-V[l]/(k*p)^2
Length[l]<-p
Width[l]<-k

x[l] <- k*p
V[l]<- V[l]/(n*m-1)
CV[l]<-sqrt(V[l])*100/media
}
}
z<-log(x[-1])
y<-log(V[1]/V[-1])
model <-lm(y ~ 0+z)
b <- as.numeric(coef(model))
tabla <-cbind(Size=x,Width,Length,plots=n*m/x,Vx=V,CV=round(CV,1))
uniformity<-tabla[order(tabla[,1]),]
model <- lm(CV ~ I(log(x)))
coeff <- coef(model)
size<-1:max(x)
cv<- coeff[1]+coeff[2]*log(size)
if(PLOT){
plot(size,cv,...)
points(x,CV)
}
invisible(list(model=model,uniformity=uniformity))
}
