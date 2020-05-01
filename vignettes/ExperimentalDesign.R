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

