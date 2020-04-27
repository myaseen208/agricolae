#' Strip-Plot analysis
#' 
#' The variance analysis of a strip-plot design is divided into three parts:
#' the horizontal-factor analysis, the vertical-factor analysis, and the
#' interaction analysis.
#' 
#' The strip-plot design is specifically suited for a two-factor experiment in
#' which the desired precision for measuring the interaction effects between
#' the two factors is higher than that for measuring the main efect two factors
#' 
#' @param BLOCK replications
#' @param COL Factor column
#' @param ROW Factor row
#' @param Y Variable, response
#' @return Data and analysis of the variance of the strip plot design.
#' @author Felipe de Mendiburu
#' @seealso \code{\link{ssp.plot}}, \code{\link{sp.plot}},
#' \code{\link{design.split}}, \code{\link{design.strip} }
#' @references Statistical procedures for agricultural research.  Kwanchai A.
#' Gomez, Arturo A. Gomez. Second Edition. 1984.
#' @keywords models
#' @export
#' @examples
#' 
#' # Yield
#' library(agricolae)
#' data(huasahuasi)
#' YIELD<-huasahuasi$YIELD
#' market <- YIELD$y1da + YIELD$y2da
#' non_market <- YIELD$y3da
#' yield <- market + non_market
#' model<-with(YIELD,strip.plot(block, clon, trt, yield))
#' out1<-with(YIELD,LSD.test(yield,clon,model$gl.a,model$Ea))
#' oldpar<-par(mar=c(3,8,1,1),cex=0.8)
#' plot(out1,xlim=c(0,80),horiz=TRUE,las=1)
#' out2<-with(YIELD,LSD.test(yield,trt,model$gl.b,model$Eb))
#' plot(out2,xlim=c(0,80),horiz=TRUE,las=1)
#' par(oldpar)
#' 
strip.plot <-
function (BLOCK, COL, ROW, Y)
{
name.y <- paste(deparse(substitute(Y)))
name.col <- paste(deparse(substitute(COL)))
name.row <- paste(deparse(substitute(ROW)))
name.r <- paste(deparse(substitute(BLOCK)))
cat("\nANALYSIS STRIP PLOT: ", name.y, "\nClass level information\n\n")
COL <- as.factor(COL)
ROW <- as.factor(ROW)
BLOCK <- as.factor(BLOCK)
nrep <- length(unique(BLOCK))
nCOL <- length(unique(COL))
nROW <- length(unique(ROW))
data <-data.frame(BLOCK, COL, ROW, Y)
names(data)<- c(name.r ,name.col,name.row, name.y)
cat(name.col,  "\t: ",unique(as.character(COL)),"\n")
cat(name.row,  "\t: ",unique(as.character(ROW)),"\n")
cat(name.r,    "\t: ",unique(as.character(BLOCK)),"\n")
cat("\nNumber of observations: ", length(Y), "\n\n")
model <-aov(Y~BLOCK+COL+  BLOCK:COL + ROW + BLOCK:ROW+ COL:ROW)
mm <- anova(model)
nn <- mm[3, ]
nn1<- row.names(mm)[3]
nn2<- row.names(mm)[4]
row.names(mm)[4] <- " "
mm[3, ] <- mm[4, ]
mm[4, ] <- nn
row.names(mm)[3] <- nn2
row.names(mm)[4] <- nn1
mm[2, 4] <- mm[2, 3]/mm[3, 3]
mm[2, 5] <- 1 - pf(mm[2, 4], mm[2, 1], mm[3, 1])
mm[4, 4] <- mm[4, 3]/mm[5, 3]
mm[4, 5] <- 1 - pf(mm[4, 4], mm[4, 1], mm[5, 1])
N<-NULL
N[1]<- name.r
N[2]<- name.col
N[3]<- "Ea"
N[4]<- name.row
N[5]<- "Eb"
N[6]<- paste(name.row,":",name.col,sep="")
N[7]<- "Ec"
NN<-paste(name.y,"~",N[1],"+",N[2], "+ Ea +",N[4],"+ Eb +",N[6],"+ Ec")
cat("model Y:",NN,"\n\n")
rownames(mm)<-N
attributes(mm)$heading[2]<-paste("Response:",name.y)
print(mm)
DFE <- df.residual(model)
MSE <- deviance(model)/DFE
medy <- mean(Y,na.rm=TRUE)
gl.a<-mm[3,1]; Ea<-mm[3,3]
gl.b<-mm[5,1]; Eb<-mm[5,3]
gl.c<-mm[7,1]; Ec<-mm[7,3]
cat("\ncv(a) =",round(sqrt(Ea)*100/medy,1),"%,", 
"cv(b) =",round(sqrt(Eb)*100/medy,1),"%,",
"cv(c) =",round(sqrt(Ec)*100/medy,1),"%,","Mean =", medy,"\n\n")
output<-list(data=data,ANOVA=mm, gl.a=gl.a,gl.b=gl.b,gl.c=gl.c,Ea=Ea,Eb=Eb,Ec=Ec)
invisible(output)
}
