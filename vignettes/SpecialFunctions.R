## ----echo=FALSE--------------------------------------------------------
out_type <- knitr::opts_knit$get("rmarkdown.pandoc.to")

r = getOption("repos")
r["CRAN"] = "https://cran.rstudio.com/"
#r["CRAN"] = "https://cloud.r-project.org/"
#r["CRAN"] = "https://ftp.iitm.ac.in/cran/"
options(repos = r)

## ----results='asis', echo=FALSE----------------------------------------
switch(out_type,
    html = {cat("<p>1. Professor of the Academic Department of Statistics and Informatics of the Faculty of Economics and Planning.National University Agraria La Molina-PERU.</p>
    
<p>2. Department of Mathematics and Statistics, University of Agriculture Faisalabad, Pakistan.</p>")},
    latex = cat("\\begin{center}
1. Professor of the Academic Department of Statistics and Informatics of the Faculty of Economics and Planning.National University Agraria La Molina-PERU.

2. Department of Mathematics and Statistics, University of Agriculture Faisalabad, Pakistan.

\\end{center}" )
)

## ----include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
    echo    = TRUE
  , comment = ""
  , fig.cap = ""
  )
library(agricolae)

## ----f7, fig=TRUE,width=5, height=2------------------------------------
oldpar<-par(cex=0.6,mar=c(3,3,2,1))
data(pamCIP)
rownames(pamCIP)<-substr(rownames(pamCIP),1,6)
output<-consensus(pamCIP,distance="binary", method="complete", nboot=5)
par(oldpar)

## ----f8, fig=TRUE,width=5, height=2------------------------------------
oldpar<-par(cex=0.6,mar=c(3,3,1.5,1))
out1<- hcut(output,h=0.4,group=8,type="t",edgePar = list(lty=1:2, col=colors()[c(42,84)]),
main="group 8" ,col.text="blue",cex.text=1,las=1)
par(oldpar)

## ----------------------------------------------------------------------
names(output)

## ----------------------------------------------------------------------
dend <- as.dendrogram(output$dendrogram)
data <- output$table.dend
head(output$table.dend)

## ----eval=FALSE--------------------------------------------------------
#  oldpar<-par(mar=c(3,3,1,1),cex=0.6)
#  plot(dend,type="r",edgePar = list(lty=1:2, col=colors()[c(42,84)]) ,las=1)
#  text(data[,3],data[,4],data[,5],col="blue",cex=1)
#  par(oldpar)

## ----------------------------------------------------------------------
data(soil)
# set.seed(9473)
simulated <- montecarlo(soil$pH,1000)
h<-graph.freq(simulated,nclass=7,plot=FALSE)

## ----f9, fig=TRUE, width=4, height=1.5---------------------------------
oldpar<-par(mar=c(2,0,2,1),cex=0.6)
plot(density(soil$pH),axes=FALSE,main="pH density of the soil\ncon Ralstonia",xlab="",lwd=4)
lines(density(simulated), col="blue", lty=4,lwd=4)
axis(1,0:12)
legend("topright",c("Original","Simulated"),lty=c(1,4),col=c("black", "blue"), lwd=4)
par(oldpar)

## ----------------------------------------------------------------------
round(table.freq(h),2)

## ----------------------------------------------------------------------
summary(soil$pH)

## ----------------------------------------------------------------------
summary(simulated)

## ----------------------------------------------------------------------
data(potato)
potato[,1]<-as.factor(potato[,1])
potato[,2]<-as.factor(potato[,2])
model<-"cutting~variety + date + variety:date"
analysis<-resampling.model(model, potato, k=100)
Xsol<-as.matrix(round(analysis$solution,2))
print(Xsol,na.print = "")

## ----------------------------------------------------------------------
simModel <- simulation.model(model, potato, k=100,console=TRUE)

## ----include=FALSE,echo=FALSE------------------------------------------
ab<-simModel$simulation[3,3]

## ----------------------------------------------------------------------
corr.x<- matrix(c(1,0.5,0.5,1),c(2,2))
corr.y<- rbind(0.6,0.7)
names<-c("X1","X2")
dimnames(corr.x)<-list(names,names)
dimnames(corr.y)<-list(names,"Y")
output<-path.analysis(corr.x,corr.y)

## ----------------------------------------------------------------------
output

## ----------------------------------------------------------------------
rm(list=ls())
options(digits = 2)
data(heterosis)
str(heterosis)
site2<-subset(heterosis,heterosis[,1]==2)
site2<-subset(site2[,c(2,5,6,8)],site2[,4]!="Control")
output1<-with(site2,lineXtester(Replication, Female, Male, v2))
options(digits = 7)

## ----f10, fig=TRUE, width=4, height=2----------------------------------
oldpar<-par(mar=c(3,3,4,1),cex=0.7)
data(rice)
table<-index.smith(rice, col="blue",
 main="Interaction between the CV and the plot size",type="l",xlab="Size")
par(oldpar)

## ----------------------------------------------------------------------
uniformity <- data.frame(table$uniformity)
head(uniformity)

## ----------------------------------------------------------------------
data(paracsho)
species <- paracsho[79:87,4:6]
species

## ----------------------------------------------------------------------
output <- index.bio(species[,3],method="Shannon",level=95,nboot=200)

## ----------------------------------------------------------------------
data(soil)
correlation(soil[,2:4],method="pearson")
with(soil,correlation(pH,soil[,3:4],method="pearson"))

## ----------------------------------------------------------------------
data(RioChillon)
with(RioChillon$babies,tapply.stat(yield,farmer,function(x) max(x)-min(x)))

## ----------------------------------------------------------------------
data(sweetpotato)
model <- model<-aov(yield ~ virus, data=sweetpotato)
cv.model(model)

## ----------------------------------------------------------------------
x<-c(3,4,5,2,3,4,5,6,4,NA,7)

## ----------------------------------------------------------------------
skewness(x)

## ----------------------------------------------------------------------
kurtosis(x)

## ----------------------------------------------------------------------
q<-5
f<-15
K<-seq(10,1000,100)
n<-length(K)
y<-rep(0,3*n)
dim(y)<-c(n,3)
for(i in 1:n) y[i,1]<-waller(K[i],q,f,Fc=2)
for(i in 1:n) y[i,2]<-waller(K[i],q,f,Fc=4)
for(i in 1:n) y[i,3]<-waller(K[i],q,f,Fc=8)

## ----eval=FALSE--------------------------------------------------------
#  oldpar<-par(mar=c(3,3,4,1),cex=0.7)
#  plot(K,y[,1],type="l",col="blue",ylab="waller",bty="l")
#  lines(K,y[,2],type="l",col="brown",lty=2,lwd=2)
#  lines(K,y[,3],type="l",col="green",lty=4,lwd=2)
#  legend("topleft",c("2","4","8"),col=c("blue","brown","green"),lty=c(1,8,20),
#  lwd=2,title="Fc")
#  title(main="Waller in function of K")
#  par(oldpar)

## ----------------------------------------------------------------------
K<-100
Fc<-1.2
q<-c(seq(6,20,1),30,40,100)
f<-c(seq(4,20,2),24,30)
n<-length(q)
m<-length(f)
W.D <-rep(0,n*m)
dim(W.D)<-c(n,m)
for (i in 1:n) {
for (j in 1:m) {
W.D[i,j]<-waller(K, q[i], f[j], Fc)
}}
W.D<-round(W.D,2)
dimnames(W.D)<-list(q,f)
cat("table: Waller Duncan k=100, F=1.2")
print(W.D)

## ----------------------------------------------------------------------
days<-c(7,14,21,28,35,42)
evaluation<-data.frame(E1=10,E2=40,E3=50,E4=70,E5=80,E6=90)
print(evaluation)
absolute1 <-audpc(evaluation,days)
relative1 <-round(audpc(evaluation,days,"relative"),2)

## ----------------------------------------------------------------------
absolute2 <-audps(evaluation,days)
relative2 <-round(audps(evaluation,days,"relative"),2)

## ----f11, echo=FALSE, fig=TRUE, width=8, height=2----------------------
oldpar<-par(mfrow=c(1,2),mar=c(3,3,1,1),cex=0.7)
plot(days, evaluation,type="h",ylim=c(0,100),axes=FALSE,col= colors()[42],xlab="Days", ylab="Evaluation")
lines(days,evaluation,col= colors()[42])
axis(1,days)
axis(2,seq(0,100,20),las=2)
rect(7,0,42,100)
text(15,80,substitute(paste("Audpc Abs.=",A1),list(A1=absolute1)))
text(15,70,substitute(paste("Audpc Rel.=",A2),list(A2=relative1)))
x<-seq(3.5,45.5,7)
evaluation<-c(evaluation,evaluation[6])
plot(x, evaluation,type="s",ylim=c(0,100),axes=FALSE,col= colors()[42],xlab="Days", ylab="Evaluation")
points(x,evaluation,type="h",col= colors()[42])
points(days,evaluation[-6],pch=16,col=4)
text(days,5,days,col=4,cex=1)
axis(1,x,pos=0)
axis(2,seq(0,100,20),las=2)
rect(3.5,0,45.5,100)
text(13,80,substitute(paste("Audps Abs.=",A1),list(A1=absolute2)))
text(13,70,substitute(paste("Audps Rel.=",A2),list(A2=relative2)))

## ----------------------------------------------------------------------
data(potato)
potato[,1]<-as.factor(potato[,1])
model<-lm(cutting ~ date + variety,potato)
df<-df.residual(model)
MSerror<-deviance(model)/df
analysis<-with(potato,nonadditivity(cutting, date, variety, df, MSerror))

## ----------------------------------------------------------------------
options(digits=2)
f <- system.file("external/weather.csv", package="agricolae")
weather <- read.csv(f,header=FALSE)
f <- system.file("external/severity.csv", package="agricolae")
severity <- read.csv(f)
weather[,1]<-as.Date(weather[,1],format = "%m/%d/%Y")
# Parameters dates
dates<-c("2000-03-25","2000-04-09","2000-04-12","2000-04-16","2000-04-22")
dates<-as.Date(dates)
EmergDate <- as.Date("2000/01/19")
EndEpidDate <- as.Date("2000-04-22")
dates<-as.Date(dates)
NoReadingsH<- 1
RHthreshold <- 90
WS<-weatherSeverity(weather,severity,dates,EmergDate,EndEpidDate,
NoReadingsH,RHthreshold)
# Parameters to Lateblight function
InocDate<-"2000-03-18"
LGR <- 0.00410
IniSpor <- 0
SR <- 292000000
IE <- 1.0
LP <- 2.82
InMicCol <- 9
Cultivar <- "NICOLA"
ApplSys <- "NOFUNGICIDE"
main<-"Cultivar: NICOLA"

## ----f12, fig=TRUE, width=4.5, height=2.5------------------------------
oldpar<-par(mar=c(3,3,4,1),cex=0.7)
#--------------------------
model<-lateblight(WS, Cultivar,ApplSys, InocDate, LGR,IniSpor,SR,IE, 
LP,MatTime='LATESEASON',InMicCol,main=main,type="l",xlim=c(65,95),lwd=1.5,
xlab="Time (days after emergence)", ylab="Severity (Percentage)")
par(oldpar)

## ----------------------------------------------------------------------
head(model$Gfile)
str(model$Ofile)
head(model$Ofile[,1:7])

## ----------------------------------------------------------------------
x<- model$Ofile$nday
y<- model$Ofile$SimSeverity
w<- model$Gfile$nday
z<- model$Gfile$MeanSeverity
Min<-model$Gfile$MinObs
Max<-model$Gfile$MaxObs

## ----eval=FALSE--------------------------------------------------------
#  oldpar<-par(mar=c(3,2.5,1,1),cex=0.7)
#  plot(x,y,type="l",xlim=c(65,95),lwd=1.5,xlab="Time (days after emergence)",
#  ylab="Severity (Percentage)")
#  points(w,z,col="red",cex=1,pch=19); npoints <- length(w)
#  for ( i in 1:npoints)segments(w[i],Min[i],w[i],Max[i],lwd=1.5,col="red")
#  legend("topleft",c("Disease progress curves","Weather-Severity"),
#  title="Description",lty=1,pch=c(3,19),col=c("black","red"))
#  par(oldpar)

## ----------------------------------------------------------------------
options(digit=2)
f <- system.file("external/dataStb.csv", package="agricolae")
dataStb<-read.csv(f)
stability.par(dataStb, rep=4, MSerror=1.8, alpha=0.1, main="Genotype",console=TRUE)

## ----------------------------------------------------------------------
output <- stability.par(dataStb, rep=4, MSerror=2)
names(output)
print(output$stability)

## ----------------------------------------------------------------------
data5<-dataStb[,1:5]
altitude<-c(1200, 1300, 800, 1600, 2400)
stability <- stability.par(data5,rep=4,MSerror=2, cova=TRUE, name.cov= "altitude",
file.cov=altitude)

## ----------------------------------------------------------------------
data <- data.frame(name=row.names(dataStb), dataStb)
output<-stability.nonpar(data, "YIELD", ranking=TRUE)
names(output)
output$statistics

## ----------------------------------------------------------------------
  str(AMMI)

## ----eval=FALSE--------------------------------------------------------
#    str(plot.AMMI)

## ----------------------------------------------------------------------
data(plrv)
model<-with(plrv,AMMI(Locality, Genotype, Rep, Yield, console=FALSE))
names(model)
model$ANOVA
model$analysis
pc <- model$analysis[, 1]
pc12<-sum(pc[1:2])
pc123<-sum(pc[1:3])

## ----f6, fig=TRUE,width=3, height=2,results="hide"---------------------
oldpar<-par(cex=0.4,mar=c(4,4,1,2))
plot(model,type=1,las=1,xlim=c(-5,6))
par(oldpar)

## ----------------------------------------------------------------------
data(plrv)
model<- with(plrv,AMMI(Locality, Genotype, Rep, Yield, console=FALSE))
index<-index.AMMI(model)
# Crops with improved stability according AMMI.
print(index[order(index[,3]),])
# Crops with better response and improved stability according AMMI.
print(index[order(index[,4]),])

