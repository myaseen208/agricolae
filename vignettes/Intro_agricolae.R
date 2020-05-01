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
    latex = cat("
1. Professor of the Academic Department of Statistics and Informatics of the Faculty of Economics and Planning.National University Agraria La Molina-PERU.

2. Department of Mathematics and Statistics, University of Agriculture Faisalabad, Pakistan.
" )
)

## ----include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
    echo    = TRUE
  , comment = ""
  , fig.cap = ""
  )
library(agricolae)

## ----eval=FALSE--------------------------------------------------------
#  install.packages("agricolae")

## ----results = "hide"--------------------------------------------------
library(agricolae)

## ----eval=FALSE--------------------------------------------------------
#  help(package="agricolae")
#  help(waller.test)

## ----------------------------------------------------------------------
detach(package:agricolae) # detach package agricole
library(agricolae) # Load the package to the memory
designs<-apropos("design")
print(designs[substr(designs,1,6)=="design"], row.names=FALSE)

## ----eval=FALSE--------------------------------------------------------
#  For the use of symbols that do not appear in the keyboard in Spanish, such as:
#  
#  ~, [, ], &, ^, |. <, >, {, }, \% or others, use the table ASCII code.

## ----------------------------------------------------------------------
library(agricolae) # Load the package to the memory: 

## ----eval=FALSE--------------------------------------------------------
#  help(graph.freq)
#  ? (graph.freq)
#  str(normal.freq)
#  example(join.freq)

## ----------------------------------------------------------------------
A<-as.data.frame(data(package="agricolae")$results[,3:4])
A[,2]<-paste(substr(A[,2],1,35),"..",sep=".")
head(A)

## ----DescriptStats1----------------------------------------------------
weight<-c( 68, 53, 69.5, 55, 71, 63, 76.5, 65.5, 69, 75, 76, 57, 70.5, 71.5, 56, 81.5,
           69, 59, 67.5, 61, 68, 59.5, 56.5, 73, 61, 72.5, 71.5, 59.5, 74.5, 63)
print(summary(weight)) 

## ----DescriptStats2, fig=TRUE, width=6, height=2, fig.cap="Absolute and relative frequency with polygon"----
oldpar<-par(mfrow=c(1,2),mar=c(4,4,0,1),cex=0.6)
h1<- graph.freq(weight,col=colors()[84],frequency=1,las=2, ylim=c(0,12),ylab="Frequency") 
x<-h1$breaks
h2<- plot(h1, frequency =2, axes= FALSE,ylim=c(0,0.4),xlab="weight",ylab="Relative (%)")
polygon.freq(h2, col=colors()[84], lwd=2, frequency =2)
axis(1,x,cex=0.6,las=2)
y<-seq(0,0.4,0.1)
axis(2, y,y*100,cex=0.6,las=1) 
par(oldpar)

## ----DescriptStats3----------------------------------------------------
stat.freq(h1)

## ----DescriptStats4----------------------------------------------------
print(summary(h1),row.names=FALSE)

## ----DescriptStats5----------------------------------------------------
sturges.freq(weight)
intervals.freq(h1)
join.freq(h1,1:3) -> h3
print(summary(h3))

## ----DescriptStats6,fig=TRUE  ,width=6, height=2, fig.cap="Join frequency and relative frequency with normal and Ogive"----
oldpar<-par(mfrow=c(1,2),mar=c(4,4,0,1),cex=0.8)
plot(h3, frequency=2,col=colors()[84],ylim=c(0,0.6),axes=FALSE,xlab="weight",ylab="%")
y<-seq(0,0.6,0.2)
axis(2,y,y*100,las=2)
axis(1,h3$breaks)
normal.freq(h3,frequency=2,col=colors()[90])
ogive.freq(h3,col=colors()[84],xlab="weight")
par(oldpar)

## ----DescriptStats7,fig=TRUE,  width=6, height=1.7, fig.cap="hist() function and histogram defined class"----
oldpar<-par(mfrow=c(1,2),mar=c(4,3,2,1),cex=0.6)
h4<-hist(weight,xlab="Classes (h4)")
table.freq(h4)
# this is possible
# hh<-graph.freq(h4,plot=FALSE)
# summary(hh)
# new class
classes <- c(0, 10, 20, 30, 40, 50)
freq <- c(3, 8, 15, 18, 6)
h5 <- graph.freq(classes,counts=freq, xlab="Classes (h5)",main="Histogram grouped data")
par(oldpar)

## ----DescriptStats8----------------------------------------------------
print(summary(h5),row.names=FALSE)

## ----CRD1--------------------------------------------------------------
str(design.crd)

## ----CRD2--------------------------------------------------------------
trt <- c("A", "B", "C")
repeticion <- c(4, 3, 4)
outdesign <- design.crd(trt,r=repeticion,seed=777,serie=0)
book1 <- outdesign$book
head(book1)

## ----RCBD1-------------------------------------------------------------
str(design.rcbd)

## ----RCBD2-------------------------------------------------------------
trt <- c("A", "B", "C","D","E")
repeticion <- 4
outdesign <- design.rcbd(trt,r=repeticion, seed=-513, serie=2)
# book2 <- outdesign$book
book2<- zigzag(outdesign) # zigzag numeration
print(outdesign$sketch)
print(matrix(book2[,1],byrow = TRUE, ncol = 5))

## ----Latin1------------------------------------------------------------
str(design.lsd)

## ----Latin2------------------------------------------------------------
trt <- c("A", "B", "C", "D")
outdesign <- design.lsd(trt, seed=543, serie=2)
print(outdesign$sketch)

## ----Latin3------------------------------------------------------------
book <- zigzag(outdesign)
print(matrix(book[,1],byrow = TRUE, ncol = 4))

## ----GLD1--------------------------------------------------------------
str(design.graeco)

## ----GLD2--------------------------------------------------------------
trt1 <- c("A", "B", "C", "D")
trt2 <- 1:4
outdesign <- design.graeco(trt1,trt2, seed=543, serie=2)
print(outdesign$sketch)

## ----GLD3--------------------------------------------------------------
book <- zigzag(outdesign)
print(matrix(book[,1],byrow = TRUE, ncol = 4))

## ----YSD1--------------------------------------------------------------
str(design.youden)

## ----YSD2--------------------------------------------------------------
varieties<-c("perricholi","yungay","maria bonita","tomasa")
r<-3
outdesign <-design.youden(varieties,r,serie=2,seed=23)
print(outdesign$sketch)
book <- outdesign$book
print(book) # field book.
print(matrix(as.numeric(book[,1]),byrow = TRUE, ncol = r))

## ----YSD3--------------------------------------------------------------
book <- zigzag(outdesign)
print(matrix(as.numeric(book[,1]),byrow = TRUE, ncol = r))

## ----BIBD1-------------------------------------------------------------
str(design.bib)

## ----BIBD2-------------------------------------------------------------
trt <- c("A", "B", "C", "D", "E" )
k <- 4
outdesign <- design.bib(trt,k, seed=543, serie=2)
book5 <- outdesign$book
outdesign$statistics
outdesign$parameters

## ----BIBD3-------------------------------------------------------------
outdesign$sketch

## ----------------------------------------------------------------------
book <- zigzag(outdesign)
matrix(book[,1],byrow = TRUE, ncol = 4)

## ----CD1---------------------------------------------------------------
str(design.cyclic)

## ----CD2---------------------------------------------------------------
trt <- c("A", "B", "C", "D", "E", "F" )
outdesign <- design.cyclic(trt,k=3, r=6, seed=543, serie=2)
book6 <- outdesign$book
outdesign$sketch[[1]]
outdesign$sketch[[2]]

## ----CD3---------------------------------------------------------------
book <- zigzag(outdesign)
array(book$plots,c(3,6,2))->X
t(X[,,1])
t(X[,,2])

## ----LD1---------------------------------------------------------------
str(design.lattice)

## ----LD2---------------------------------------------------------------
trt<-letters[1:9]
outdesign <-design.lattice(trt, r = 3, serie = 2, seed = 33, 
    kinds =  "Super-Duper")
book7 <- outdesign$book
outdesign$parameters
outdesign$sketch
head(book7)

## ----LD3---------------------------------------------------------------
book <- zigzag(outdesign)
array(book$plots,c(3,3,3)) -> X
t(X[,,1])
t(X[,,2])
t(X[,,3])

## ----Alpha1------------------------------------------------------------
str(design.alpha)

## ----Alpha2------------------------------------------------------------
trt <- letters[1:15]
outdesign <- design.alpha(trt,k=3,r=2,seed=543)
book8 <- outdesign$book
outdesign$statistics
outdesign$sketch
# codification of the plots
A<-array(book8[,1], c(3,5,2))
t(A[,,1])
t(A[,,2])

## ----Alpha3------------------------------------------------------------
book <- zigzag(outdesign)
A<-array(book[,1], c(3,5,2))
t(A[,,1])
t(A[,,2])

## ----ABD1--------------------------------------------------------------
str(design.dau)

## ----ABD2--------------------------------------------------------------
rm(list=ls())
trt1 <- c("A", "B", "C", "D")
trt2 <- c("t","u","v","w","x","y","z")
outdesign <- design.dau(trt1, trt2, r=5, seed=543, serie=2)
book9 <- outdesign$book
with(book9,by(trt, block,as.character))

## ----ABD3--------------------------------------------------------------
book <- zigzag(outdesign)
with(book,by(plots, block, as.character))
head(book)

## ----SPD1--------------------------------------------------------------
str(design.split)

## ----SPD2--------------------------------------------------------------
trt1<-c("A","B","C","D")
trt2<-c("a","b","c")
outdesign <-design.split(trt1,trt2,r=3,serie=2,seed=543)
book10 <- outdesign$book
head(book10)
p<-book10$trt1[seq(1,36,3)]
q<-NULL
for(i in 1:12) 
q <- c(q,paste(book10$trt2[3*(i-1)+1],book10$trt2[3*(i-1)+2], book10$trt2[3*(i-1)+3]))

## ----SPD3--------------------------------------------------------------
print(t(matrix(p,c(4,3))))

## ----SPD4--------------------------------------------------------------
print(t(matrix(q,c(4,3))))

## ----SPD5--------------------------------------------------------------
book <- zigzag(outdesign)
head(book,5)

## ----StPD1-------------------------------------------------------------
str(design.strip)

## ----StPD2-------------------------------------------------------------
trt1<-c("A","B","C","D")
trt2<-c("a","b","c")
outdesign <-design.strip(trt1,trt2,r=3,serie=2,seed=543)
book11 <- outdesign$book
head(book11)
t3<-paste(book11$trt1, book11$trt2)
B1<-t(matrix(t3[1:12],c(4,3)))
B2<-t(matrix(t3[13:24],c(3,4)))
B3<-t(matrix(t3[25:36],c(3,4)))
print(B1)
print(B2)
print(B3)

## ----StPD3-------------------------------------------------------------
book <- zigzag(outdesign)
head(book)
array(book$plots,c(3,4,3))->X
t(X[,,1])
t(X[,,2])
t(X[,,3])

## ----Factorial1--------------------------------------------------------
str(design.ab)

## ----Factorial2--------------------------------------------------------
trt <- c (4,2,3) # three factors with  4,2 and 3 levels.

## ----Factorial3--------------------------------------------------------
trt<-c(3,2) # factorial 3x2
outdesign <-design.ab(trt, r=3, serie=2)
book12 <- outdesign$book
head(book12) # print of the field book

## ----Factorial4--------------------------------------------------------
book <- zigzag(outdesign)
head(book)

## ----Factorial5--------------------------------------------------------
trt<-c(2,2,2)
crd<-design.ab(trt, r=5, serie=2,design="crd")
names(crd)
crd$parameters
head(crd$book)

## ----------------------------------------------------------------------
data(sweetpotato)
model<-aov(yield~virus, data=sweetpotato)
cv.model(model)
with(sweetpotato,mean(yield))

## ----------------------------------------------------------------------
df<-df.residual(model)
MSerror<-deviance(model)/df

## ----------------------------------------------------------------------
# comparison <- LSD.test(yield,virus,df,MSerror)
LSD.test(model, "virus",console=TRUE)

## ----------------------------------------------------------------------
# comparison <- LSD.test(yield, virus,df, MSerror, group=FALSE)
outLSD <-LSD.test(model, "virus", group=FALSE,console=TRUE)

## ----------------------------------------------------------------------
options(digits=2)
print(outLSD)

## ----------------------------------------------------------------------
LSD.test(model, "virus", group=FALSE, p.adj= "bon",console=TRUE)
out<-LSD.test(model, "virus", group=TRUE, p.adj= "holm")
print(out$group)
out<-LSD.test(model, "virus", group=FALSE, p.adj= "holm")
print(out$comparison)

## ----------------------------------------------------------------------
duncan.test(model, "virus",console=TRUE)

## ----------------------------------------------------------------------
# SNK.test(model, "virus", alpha=0.05,console=TRUE)
SNK.test(model, "virus", group=FALSE,console=TRUE)

## ----------------------------------------------------------------------
# REGW.test(model, "virus", alpha=0.05,console=TRUE)
REGW.test(model, "virus", group=FALSE,console=TRUE)

## ----------------------------------------------------------------------
outHSD<- HSD.test(model, "virus",console=TRUE)
outHSD

## ----------------------------------------------------------------------
A<-sweetpotato[-c(4,5,7),]
modelUnbalanced <- aov(yield ~ virus, data=A)
outUn <-HSD.test(modelUnbalanced, "virus",group=FALSE, unbalanced = TRUE)
print(outUn$comparison)
outUn <-HSD.test(modelUnbalanced, "virus",group=TRUE, unbalanced = TRUE)
print(outUn$groups)

## ----------------------------------------------------------------------
outUn <-HSD.test(modelUnbalanced, "virus",group=FALSE)
print(outUn$comparison[,1:2])
outUn <-HSD.test(modelUnbalanced, "virus",group=TRUE)
print(outUn$groups)

## ----------------------------------------------------------------------
# variance analysis:
anova(model)
with(sweetpotato,waller.test(yield,virus,df,MSerror,Fc= 17.345, group=FALSE,console=TRUE))

## ----------------------------------------------------------------------
outWaller <- waller.test(model, "virus", group=FALSE,console=FALSE)

## ----------------------------------------------------------------------
names(outWaller)
print(outWaller$comparison)

## ----------------------------------------------------------------------
outWaller$statistics

## ----------------------------------------------------------------------
# analysis of variance: 
scheffe.test(model,"virus", group=TRUE,console=TRUE,
main="Yield of sweetpotato\nDealt with different virus")

## ----------------------------------------------------------------------
outScheffe <- scheffe.test(model,"virus", group=FALSE, console=TRUE)

## ----------------------------------------------------------------------
# modelABC <-aov (y ~ A * B * C, data)
# compare <-LSD.test (modelABC, c ("A", "B", "C"),console=TRUE)

## ----------------------------------------------------------------------
yield <-scan (text =
 "6 7 9 13 16 20 8 8 9
  7 8 8 12 17 18 10 9 12
  9 9 9 14 18 21 11 12 11
  8 10 10 15 16 22 9 9 9 "
 )
block <-gl (4, 9)
clone <-rep (gl (3, 3, labels = c ("c1", "c2", "c3")), 4)
nitrogen <-rep (gl (3, 1, labels = c ("n1", "n2", "n3")), 12)
A <-data.frame (block, clone, nitrogen, yield)
head (A)
outAOV <-aov (yield ~ block + clone * nitrogen, data = A)

## ----------------------------------------------------------------------
anova (outAOV)
outFactorial <-LSD.test (outAOV, c("clone", "nitrogen"), 
main = "Yield ~ block + nitrogen + clone + clone:nitrogen",console=TRUE)

## ----eval=FALSE--------------------------------------------------------
#  oldpar<-par(mar=c(3,3,2,0))
#  pic1<-bar.err(outFactorial$means,variation="range",ylim=c(5,25), bar=FALSE,col=0,las=1)
#  points(pic1$index,pic1$means,pch=18,cex=1.5,col="blue")
#  axis(1,pic1$index,labels=FALSE)
#  title(main="average and range\nclon:nitrogen")
#  par(oldpar)

## ----------------------------------------------------------------------
# Example linear estimation and design of experiments. (Joshi)
# Institute of Social Sciences Agra, India
# 6 varieties of wheat in 10 blocks of 3 plots each.
block<-gl(10,3)
variety<-c(1,2,3,1,2,4,1,3,5,1,4,6,1,5,6,2,3,6,2,4,5,2,5,6,3,4,5,3, 4,6)
Y<-c(69,54,50,77,65,38,72,45,54,63,60,39,70,65,54,65,68,67,57,60,62, 
59,65,63,75,62,61,59,55,56)
head(cbind(block,variety,Y))
BIB.test(block, variety, Y,console=TRUE)

## ----------------------------------------------------------------------
out <-BIB.test(block, trt=variety, Y, test="tukey", group=FALSE, console=TRUE)
names(out)
rm(block,variety)

## ----------------------------------------------------------------------
# alpha design 
Genotype<-paste("geno",1:30,sep="")
r<-2
k<-3
plan<-design.alpha(Genotype,k,r,seed=5)

## ----------------------------------------------------------------------
yield <-c(5,2,7,6,4,9,7,6,7,9,6,2,1,1,3,2,4,6,7,9,8,7,6,4,3,2,2,1,1, 
          2,1,1,2,4,5,6,7,8,6,5,4,3,1,1,2,5,4,2,7,6,6,5,6,4,5,7,6,5,5,4)

## ----------------------------------------------------------------------
data<-data.frame(plan$book,yield)
# The analysis: 
modelPBIB <- with(data,PBIB.test(block, Genotype, replication, yield, k=3, group=TRUE, console=TRUE))

## ----------------------------------------------------------------------
trt<-c(1,8,5,5,2,9,2,7,3,6,4,9,4,6,9,8,7,6,1,5,8,3,2,7,3,7,2,1,3,4,6,4,9,5,8,1)
yield<-c(48.76,10.83,12.54,11.07,22,47.43,27.67,30,13.78,37,42.37,39,14.46,30.69,42.01,
22,42.8,28.28,50,24,24,15.42,30,23.8,19.68,31,23,41,12.9,49.95,25,45.57,30,20,18,43.81)
sqr<-rep(gl(4,3),3)
block<-rep(1:12,3)
modelLattice<-PBIB.test(block,trt,sqr,yield,k=3,console=TRUE, method="VC")

## ----------------------------------------------------------------------
block<-c(rep("I",7),rep("II",6),rep("III",7))
trt<-c("A","B","C","D","g","k","l","A","B","C","D","e","i","A","B", "C",
"D","f","h","j")
yield<-c(83,77,78,78,70,75,74,79,81,81,91,79,78,92,79,87,81,89,96, 82)
head(data.frame(block, trt, yield))

## ----------------------------------------------------------------------
by(trt,block,as.character)

## ----------------------------------------------------------------------
by(yield,block,as.character)

## ----------------------------------------------------------------------
modelDAU<- DAU.test(block,trt,yield,method="lsd",console=TRUE)
options(digits = 2)
modelDAU$means

## ----------------------------------------------------------------------
modelDAU<- DAU.test(block,trt,yield,method="lsd",group=FALSE,console=FALSE)
head(modelDAU$comparison,8)

## ----------------------------------------------------------------------
data(corn)
str(corn)

## ----------------------------------------------------------------------
str(kruskal)

## ----------------------------------------------------------------------
outKruskal<-with(corn,kruskal(observation,method,group=TRUE, main="corn", console=TRUE))

## ----------------------------------------------------------------------
out<-with(corn,kruskal(observation,method,group=TRUE, main="corn", p.adj="holm"))
print(out$group)
out<-with(corn,kruskal(observation,method,group=FALSE, main="corn", p.adj="holm"))
print(out$comparison)

## ----------------------------------------------------------------------
str(friedman)

## ----------------------------------------------------------------------
data(grass)
out<-with(grass,friedman(judge,trt, evaluation,alpha=0.05, group=FALSE,
main="Data of the book of Conover",console=TRUE))

## ----------------------------------------------------------------------
str(waerden.test)

## ----------------------------------------------------------------------
data(sweetpotato)
outWaerden<-with(sweetpotato,waerden.test(yield,virus,alpha=0.01,group=TRUE,console=TRUE))

## ----------------------------------------------------------------------

names(outWaerden)


## ----------------------------------------------------------------------
out<-with(sweetpotato,waerden.test(yield,virus,group=FALSE,console=TRUE))

## ----------------------------------------------------------------------
str(Median.test)

## ----------------------------------------------------------------------
str(Median.test)

## ----------------------------------------------------------------------
data(sweetpotato)
outMedian<-with(sweetpotato,Median.test(yield,virus,console=TRUE))
names(outMedian)
outMedian$statistics
outMedian$medians

## ----f14, echo=TRUE, fig=TRUE, fig.cap = "Grouping of treatments and its variation, Median method", width=6, height=4----
oldpar<-par(mfrow=c(2,2),mar=c(3,3,1,1),cex=0.8)
# Graphics
bar.group(outMedian$groups,ylim=c(0,50))
bar.group(outMedian$groups,xlim=c(0,50),horiz = TRUE)
plot(outMedian)
plot(outMedian,variation="IQR",horiz = TRUE)
par(oldpar)

## ----------------------------------------------------------------------
str(durbin.test)

## ----------------------------------------------------------------------
days <-gl(7,3)
chemical<-c("A","B","D","A","C","E","C","D","G","A","F","G", "B","C","F", 
"B","E","G","D","E","F")
toxic<-c(0.465,0.343,0.396,0.602,0.873,0.634,0.875,0.325,0.330, 0.423,0.987,0.426,
0.652,1.142,0.989,0.536,0.409,0.309, 0.609,0.417,0.931)
head(data.frame(days,chemical,toxic))
out<-durbin.test(days,chemical,toxic,group=FALSE,console=TRUE,
main="Logarithm of the toxic dose")
names(out)
out$statistics

## ----------------------------------------------------------------------
# model <-aov (yield ~ fertilizer, data = field) 
# out <-LSD.test (model, "fertilizer", group = TRUE) 
# bar.group (out$group)
str(bar.group)

## ----------------------------------------------------------------------
# model <-aov (yield ~ fertilizer, data = field) 
# out <-LSD.test (model, "fertilizer", group = TRUE) 
# bar.err(out$means)
str(bar.err)

## ----f4, fig=TRUE, fig.cap = "Comparison between treatments", width=6, height=4----
oldpar<-par(mfrow=c(2,2),mar=c(3,3,2,1),cex=0.7)
c1<-colors()[480]; c2=colors()[65] 
data(sweetpotato)
model<-aov(yield~virus, data=sweetpotato)
outHSD<- HSD.test(model, "virus",console=TRUE)
bar.err(outHSD$means, variation="range",ylim=c(0,50),col=c1,las=1)
bar.err(outHSD$means, variation="IQR",horiz=TRUE, xlim=c(0,50),col=c2,las=1)
plot(outHSD, variation="range",las=1)
plot(outHSD, horiz=TRUE, variation="SD",las=1)
par(oldpar)

## ----eval=FALSE--------------------------------------------------------
#  oldpar<-par(mfrow=c(2,2),cex=0.7,mar=c(3.5,1.5,3,1))
#  C1<-bar.err(modelPBIB$means[1:7, ], ylim=c(0,9), col=0, main="C1",
#  variation="range",border=3,las=2)
#  C2<-bar.err(modelPBIB$means[8:15,], ylim=c(0,9), col=0, main="C2",
#  variation="range", border =4,las=2)
#  # Others graphic
#  C3<-bar.err(modelPBIB$means[16:22,], ylim=c(0,9), col=0, main="C3",
#  variation="range",border =2,las=2)
#  C4<-bar.err(modelPBIB$means[23:30,], ylim=c(0,9), col=0, main="C4",
#  variation="range", border =6,las=2)
#  # Lattice graphics
#  par(oldpar)
#  oldpar<-par(mar=c(2.5,2.5,1,0),cex=0.6)
#  bar.group(modelLattice$group,ylim=c(0,55),density=10,las=1)
#  par(oldpar)

## ----f13, echo=TRUE, fig=TRUE, fig.cap = "Grouping of treatments and its variation, Duncan method", width=8, height=3----
# model : yield ~ virus
# Important group=TRUE
oldpar<-par(mfrow=c(1,2),mar=c(3,3,1,1),cex=0.8)
x<-duncan.test(model, "virus", group=TRUE)
plot(x,las=1)
plot(x,variation="IQR",horiz=TRUE,las=1)
par(oldpar)

## ----f5, fig=TRUE, fig.cap = "Mean-Mean scatter plot representation of the Tukey method", width=6, height=5----
# function (x, main = NULL, color1 = "red", color2 = "blue", 
#    color3 = "black", cex.axis = 0.8, las = 1, pch = 20, 
#    bty = "l", cex = 0.8, lwd = 1, xlab = "", ylab = "", 
#   ...)
# model : yield ~ virus
# Important group=FALSE
x<-HSD.test(model, "virus", group=FALSE)
diffograph(x,cex.axis=0.9,xlab="Yield",ylab="Yield",cex=0.9)

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

## ----f6, fig=TRUE, fig.cap = "Biplot", width=3, height=2,results="hide"----
oldpar<-par(cex=0.4,mar=c(4,4,1,2))
plot(model,type=1,las=1,xlim=c(-5,6))
par(oldpar)

## ----eval=FALSE--------------------------------------------------------
#  plot(model,type=2,las=1)

## ----------------------------------------------------------------------
data(plrv)
model<- with(plrv,AMMI(Locality, Genotype, Rep, Yield, console=FALSE))
index<-index.AMMI(model)
# Crops with improved stability according AMMI.
print(index[order(index[,3]),])
# Crops with better response and improved stability according AMMI.
print(index[order(index[,4]),])

## ----f7, fig=TRUE, fig.cap = "Dendrogram, production by `consensus`", width=5, height=2----
oldpar<-par(cex=0.6,mar=c(3,3,2,1))
data(pamCIP)
rownames(pamCIP)<-substr(rownames(pamCIP),1,6)
output<-consensus(pamCIP,distance="binary", method="complete", nboot=5)
par(oldpar)

## ----f8, fig=TRUE, fig.cap = "Dendrogram, production by `hcut()`", width=5, height=2----
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

## ----eval=TRUE---------------------------------------------------------
oldpar<-par(mar=c(3,3,1,1),cex=0.6)
plot(dend,type="r",edgePar = list(lty=1:2, col=colors()[c(42,84)]) ,las=1)
text(data[,3],data[,4],data[,5],col="blue",cex=1)
par(oldpar)

## ----------------------------------------------------------------------
data(soil)
# set.seed(9473)
simulated <- montecarlo(soil$pH,1000)
h<-graph.freq(simulated,nclass=7,plot=FALSE)

## ----f9, fig=TRUE, fig.cap = "Distribution of the simulated and the original data", width=4, height=1.5----
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

## ----eval=FALSE--------------------------------------------------------
#       Replication   Female   Male   v2
#       109           1     LT-8  TS-15 2.65s
#       110           1     LT-8 TPS-13 2.26
#       ...
#       131           1 Achirana TPS-13 3.55
#       132           1 Achirana TPS-67 3.05
#       ...
#       140           1 Achirana   <NA> 3.35
#       ...
#       215           3     <NA> TPS-67 2.91

## ----------------------------------------------------------------------
rm(list=ls())
options(digits = 2)
data(heterosis)
str(heterosis)
site2<-subset(heterosis,heterosis[,1]==2)
site2<-subset(site2[,c(2,5,6,8)],site2[,4]!="Control")
output1<-with(site2,lineXtester(Replication, Female, Male, v2))
options(digits = 7)

## ----f10, fig=TRUE, fig.cap = "Adjustment curve for the optimal size of plot", width=4, height=2----
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

## ----eval = FALSE------------------------------------------------------
#  tapply.stat(A[,5:7], A[,1:3],mean)
#  tapply.stat(A[,5:7], A[,1:3],function(x) mean(x,na.rm=TRUE))
#  tapply.stat(A[,c(7,6)], A[,1:2],function(x) sd(x)*100/mean(x))

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

## ----f11, echo=FALSE, fig=TRUE, fig.cap = "Area under the curve (AUDPC) and Area under the Stairs (AUDPS)", width=8, height=2----
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

## ----f12, fig=TRUE, fig.cap = "LATESEASON", width=4.5, height=2.5------
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

