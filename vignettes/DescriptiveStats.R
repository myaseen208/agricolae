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

## ----DescriptStats1----------------------------------------------------
weight<-c( 68, 53, 69.5, 55, 71, 63, 76.5, 65.5, 69, 75, 76, 57, 70.5, 71.5, 56, 81.5,
           69, 59, 67.5, 61, 68, 59.5, 56.5, 73, 61, 72.5, 71.5, 59.5, 74.5, 63)
print(summary(weight)) 

## ----DescriptStats2, fig=TRUE, width=6, height=2, fig.cap="Absolute and relative frequency with polygon"----
oldpar<-par(mfrow=c(1,2),mar=c(4,4,0,1),cex=0.6)
h1<- graph.freq(weight,col=colors()[84],frequency=1,las=2,density=20,ylim=c(0,12),ylab="Frequency") 
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
plot(h3, frequency=2,col=colors()[84],ylim=c(0,0.6),axes=FALSE,xlab="weight",ylab="%",border=0)
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

