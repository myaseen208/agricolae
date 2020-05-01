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

