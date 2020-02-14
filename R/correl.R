correl <-
function(x,y,method = "pearson", alternative = "two.sided"){
x<-1.0*x;y<-1.0*y
n<-length(x)
if(method=="kendall"){
corr<-kendall(x,y)
stat<-corr$stat
rho<-corr$tau
if(alternative == "two.sided" ) pvalue<-corr$pvalue
if(alternative == "less" ) pvalue<-1-corr$pvalue/2
if(alternative == "greater") pvalue<-corr$pvalue/2
}
if(method=="spearman" ){
a<-rank(x)
b<-rank(y)
x<-a
y<-b
}
if ((method =="pearson") | (method=="spearman")) {
sumx<-sum(x^2)-sum(x)^2/n
sumy<-sum(y^2)-sum(y)^2/n
sumxy<-sum(x*y)-sum(x)*sum(y)/n
rho<-sumxy/sqrt(sumx*sumy)
gl<-n-2
stat<-rho*sqrt(gl)/(sqrt(1-rho^2))
if(alternative == "two.sided" ) pvalue<-2*(1-pt(abs(stat),gl))
if(alternative == "less" ) pvalue<-pt(abs(stat),gl)
if(alternative == "greater") pvalue<-1-pt(abs(stat),gl)
}
if (method =="lin") {
mx<-mean(x)
my<-mean(y)
sumx<-(sum(x^2)-sum(x)^2/n)/n
sumy<-(sum(y^2)-sum(y)^2/n)/n
sumxy<-(sum(x*y)-sum(x)*sum(y)/n)/n
r<-sumxy/sqrt(sumx*sumy)
rho<-2*sumxy/(sumx+sumy+(mx-my)^2)
gl<-n-2
sdlin<-sqrt((1/gl)*((1-r^2)*rho^2*(1-rho^2)/r^2+2*rho^3*(1-rho)*(mx-my)^2/(r*sqrt(sumx*sumy))-rho^4*(mx-my)^4/(2*sumx*sumy*r^2)))
stat<-rho/sdlin
if(alternative == "two.sided" ) pvalue<-2*(1-pt(abs(stat),gl))
if(alternative == "less" ) pvalue<-pt(abs(stat),gl)
if(alternative == "greater") pvalue<-1-pt(abs(stat),gl)
}
list(stat=stat,rho=rho,pvalue=pvalue)
}
