duncan.test <-
function (y, trt, DFerror, MSerror, alpha=0.05, group=TRUE,main = NULL,console=FALSE)
{
name.y <- paste(deparse(substitute(y)))
name.t <- paste(deparse(substitute(trt)))
if(is.null(main))main<-paste(name.y,"~", name.t)
clase<-c("aov","lm")
if("aov"%in%class(y) | "lm"%in%class(y)){
if(is.null(main))main<-y$call
A<-y$model
DFerror<-df.residual(y)
MSerror<-deviance(y)/DFerror
y<-A[,1]
ipch<-pmatch(trt,names(A))
nipch<- length(ipch)
for(i in 1:nipch){
if (is.na(ipch[i]))
return(if(console)cat("Name: ", trt, "\n", names(A)[-1], "\n"))
}
name.t<- names(A)[ipch][1]
trt <- A[, ipch]
if (nipch > 1){
trt <- A[, ipch[1]]
for(i in 2:nipch){
name.t <- paste(name.t,names(A)[ipch][i],sep=":")
trt <- paste(trt,A[,ipch[i]],sep=":")
}}
name.y <- names(A)[1]
}
junto <- subset(data.frame(y, trt), is.na(y) == FALSE)
Mean<-mean(junto[,1])
CV<-sqrt(MSerror)*100/Mean
    medians<-tapply.stat(junto[,1],junto[,2],stat="median")
    for(i in c(1,5,2:4)) {
      x <- tapply.stat(junto[,1],junto[,2],function(x)quantile(x)[i])
      medians<-cbind(medians,x[,2])
    }
    medians<-medians[,3:7]
    names(medians)<-c("Min","Max","Q25","Q50","Q75")	
means <- tapply.stat(junto[,1],junto[,2],stat="mean") # change
sds <-   tapply.stat(junto[,1],junto[,2],stat="sd") #change
nn <-   tapply.stat(junto[,1],junto[,2],stat="length") # change
means<-data.frame(means,std=sds[,2],r=nn[,2],medians)
names(means)[1:2]<-c(name.t,name.y)
ntr<-nrow(means)
Tprob<-NULL
k<-0
for(i in 2:ntr){
k<-k+1
x <- suppressWarnings(warning(qtukey((1-alpha)^(i-1), i, DFerror)))
if(x=="NaN")break
else Tprob[k]<-x
}
if(k<(ntr-1)){
for(i in k:(ntr-1)){
f <- Vectorize(function(x)ptukey(x,i+1,DFerror)-(1-alpha)^i)
Tprob[i]<-uniroot(f, c(0,100))$root
}
}
Tprob<-as.numeric(Tprob)
nr <- unique(nn[,2])
# Critical Value of Studentized Range
if(console){
cat("\nStudy:", main)
cat("\n\nDuncan's new multiple range test\nfor",name.y,"\n")
cat("\nMean Square Error: ",MSerror,"\n\n")
cat(paste(name.t,",",sep="")," means\n\n")
print(data.frame(row.names = means[,1], means[,2:6]))
}
if(length(nr) == 1 ) sdtdif <- sqrt(MSerror/nr)
else {
nr1 <-  1/mean(1/nn[,2])
sdtdif <- sqrt(MSerror/nr1)
}
DUNCAN <- Tprob * sdtdif
names(DUNCAN)<-2:ntr
duncan<-data.frame(Table=Tprob,CriticalRange=DUNCAN)
if ( group & length(nr) == 1 & console){
cat("\nAlpha:",alpha,"; DF Error:",DFerror,"\n")
cat("\nCritical Range\n")
print(DUNCAN)
}
if ( group & length(nr) != 1 & console) cat("\nGroups according to probability of means differences and alpha level(",alpha,")\n")
if ( length(nr) != 1) duncan<-NULL    
Omeans<-order(means[,2],decreasing = TRUE) #correccion 2019, 1 abril.
Ordindex<-order(Omeans)
comb <-utils::combn(ntr,2)
nn<-ncol(comb)
dif<-rep(0,nn)
DIF<-dif
LCL<-dif
UCL<-dif
pvalue<-dif
odif<-dif
sig<-NULL
for (k in 1:nn) {
i<-comb[1,k]
j<-comb[2,k]
dif[k]<-means[i,2]-means[j,2]
DIF[k]<-abs(dif[k])
nx<-abs(i-j)+1
odif[k] <- abs(Ordindex[i]- Ordindex[j])+1
pvalue[k]<- round(1-ptukey(DIF[k]/sdtdif,odif[k],DFerror)^(1/(odif[k]-1)),4)
LCL[k] <- dif[k] - DUNCAN[odif[k]-1]
UCL[k] <- dif[k] + DUNCAN[odif[k]-1]
sig[k]<-" "
if (pvalue[k] <= 0.001) sig[k]<-"***"
else  if (pvalue[k] <= 0.01) sig[k]<-"**"
else  if (pvalue[k] <= 0.05) sig[k]<-"*"
else  if (pvalue[k] <= 0.1) sig[k]<-"."
}
if(!group){  
tr.i <- means[comb[1, ],1]
tr.j <- means[comb[2, ],1]
comparison<-data.frame("difference" = dif, pvalue=pvalue,"signif."=sig,LCL,UCL)
rownames(comparison)<-paste(tr.i,tr.j,sep=" - ")
if(console){cat("\nComparison between treatments means\n\n")
print(comparison)}
groups=NULL
}
if (group) {
comparison=NULL
# The probability matrix
Q<-matrix(1,ncol=ntr,nrow=ntr)
p<-pvalue
k<-0
for(i in 1:(ntr-1)){
for(j in (i+1):ntr){
k<-k+1
Q[i,j]<-p[k]
Q[j,i]<-p[k]
}
}
groups <- orderPvalue(means[, 1], means[, 2],alpha, Q,console)
names(groups)[1]<-name.y
if(console) {
cat("\nMeans with the same letter are not significantly different.\n\n")
print(groups)
}      
}
parameters<-data.frame(test="Duncan",name.t=name.t,ntr = ntr,alpha=alpha)
statistics<-data.frame(MSerror=MSerror,Df=DFerror,Mean=Mean,CV=CV)
rownames(parameters)<-" "
rownames(statistics)<-" "
rownames(means)<-means[,1]
means<-means[,-1]
output<-list(statistics=statistics,parameters=parameters, duncan=duncan,
means=means,comparison=comparison,groups=groups)
class(output)<-"group"
invisible(output)
}
