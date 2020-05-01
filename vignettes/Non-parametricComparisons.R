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

