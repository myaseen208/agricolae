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

