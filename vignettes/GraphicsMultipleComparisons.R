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

