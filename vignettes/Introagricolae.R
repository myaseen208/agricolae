## ---- echo=FALSE------------------------------------------
out_type <- knitr::opts_knit$get("rmarkdown.pandoc.to")

r = getOption("repos")
r["CRAN"] = "https://cran.rstudio.com/"
#r["CRAN"] = "https://cloud.r-project.org/"
#r["CRAN"] = "https://ftp.iitm.ac.in/cran/"
options(repos = r)

## ---- results='asis', echo=FALSE--------------------------
switch(out_type,
    html = {cat("<p>1. Professor of the Academic Department of Statistics and Informatics of the Faculty of Economics and Planning.National University Agraria La Molina-PERU.</p>")},
    latex = cat("\\begin{center}
1. Professor of the Academic Department of Statistics and Informatics of the Faculty of Economics and Planning.National University Agraria La Molina-PERU.

\\end{center}" )
)

## ----setup, include=FALSE---------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      comment = "",
                      fig.cap = "")

## ---- eval=FALSE------------------------------------------
#  install.packages("agricolae")

## ---- results = "hide"------------------------------------
library(agricolae)

## ---- eval=FALSE------------------------------------------
#  help(package="agricolae")
#  help(waller.test)

