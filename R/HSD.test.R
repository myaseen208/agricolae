HSD.test <-
function (y, trt, DFerror, MSerror, alpha=0.05, group=TRUE,main = NULL,unbalanced=FALSE,console=FALSE)
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
#   row.names(means)<-means[,1]
ntr<-nrow(means)
Tprob <- qtukey(1-alpha,ntr, DFerror)
nr<-unique(nn[, 2])
nr1<-1/mean(1/nn[,2])
if(console){
cat("\nStudy:", main)
cat("\n\nHSD Test for",name.y,"\n")
cat("\nMean Square Error: ",MSerror,"\n\n")
cat(paste(name.t,",",sep="")," means\n\n")
print(data.frame(row.names = means[,1], means[,2:6]))
cat("\nAlpha:",alpha,"; DF Error:",DFerror,"\n")
cat("Critical Value of Studentized Range:", Tprob,"\n")
}
HSD <- Tprob * sqrt(MSerror/nr)
statistics<-data.frame(MSerror=MSerror,Df=DFerror,Mean=Mean,CV=CV,MSD=HSD)
if ( group & length(nr) == 1 & console) cat("\nMinimun Significant Difference:",HSD,"\n")
if ( group & length(nr) != 1 & console) cat("\nGroups according to probability of means differences and alpha level(",alpha,")\n")
if ( length(nr) != 1) statistics<-data.frame(MSerror=MSerror,Df=DFerror,Mean=Mean,CV=CV)
comb <-utils::combn(ntr,2)
nn<-ncol(comb)
dif<-rep(0,nn)
sig<-NULL
LCL<-dif
UCL<-dif
pvalue<-rep(0,nn)
for (k in 1:nn) {
i<-comb[1,k]
j<-comb[2,k]
#if (means[i, 2] < means[j, 2]){
#comb[1, k]<-j
#comb[2, k]<-i
#}
dif[k]<-means[i,2]-means[j,2]
sdtdif<-sqrt(MSerror * 0.5*(1/means[i,4] + 1/means[j,4]))
if(unbalanced)sdtdif<-sqrt(MSerror /nr1)
pvalue[k]<- round(1-ptukey(abs(dif[k])/sdtdif,ntr,DFerror),4)
LCL[k] <- dif[k] - Tprob*sdtdif
UCL[k] <- dif[k] + Tprob*sdtdif
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
#groups<-data.frame(trt= means[,1],means= means[,2],M="",N=means[,4],std.err=means[,3])
}
if (group) {
comparison=NULL
# Matriz de probabilidades
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
cat("\nTreatments with the same letter are not significantly different.\n\n")
print(groups)
}
}
parameters<-data.frame(test="Tukey",name.t=name.t,ntr = ntr, StudentizedRange=Tprob,alpha=alpha)
rownames(parameters)<-" "
rownames(statistics)<-" "
rownames(means)<-means[,1]
means<-means[,-1]
output<-list(statistics=statistics,parameters=parameters,
means=means,comparison=comparison,groups=groups)
class(output)<-"group"
invisible(output)
}
