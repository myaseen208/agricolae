#' Homologation of regressions
#' 
#' It makes the regressions homogeneity test for a group of treatments where
#' each observation presents a linearly dependent reply from another one.
#' There is a linear function in every treatment.  The objective is to find out
#' if the linear models of each treatment come from the same population.
#' 
#' 
#' @param trt treatment
#' @param x independent variable
#' @param y dependent variable
#' @return list objects:\cr Number regressions.\cr Residual.\cr Difference of
#' regression.\cr DF.homgeneity (homogenity degree free).\cr DF.Residual
#' (degree free error).\cr F.value. Test statitics. \cr P.value. P Value
#' (Significant\cr Criterion. conclusion\cr
#' @author Felipe de Mendiburu
#' @references Book in Spanish: Metodos estadisticos para la investigacion.
#' Calzada Benza 1960
#' @keywords regression
#' @importFrom stats var
#' @export
#' @examples
#' 
#' library(agricolae)
#' data(frijol)
#' evaluation<-with(frijol,reg.homog(technology,index,production))
#' # Example 2. Applied Regression Analysis a Research tools
#' # 1988. John O.Rawlings. Wadsworth & brooks/cole Advanced Books
#' # & Software. Pacific Grove. Califonia.
#' # Statistics/probability. Series
#' LineNumber<-c(rep("39","30"),rep("52","30"))
#' PlantingDate<-rep(c("16","20","21"),20)
#' HeadWt <- c(2.5,3.0,2.2,2.2,2.8,1.8,3.1,2.8,1.6,4.3,2.7,2.1,2.5,2.6,3.3,4.3,
#' 2.8,3.8,3.8,2.6,3.2,4.3,2.6,3.6,1.7,2.6,4.2,3.1,3.5,1.6,2.0,4.0,1.5,2.4,2.8,
#' 1.4,1.9,3.1,1.7,2.8,4.2,1.3,1.7,3.7,1.7,3.2,3.0,1.6,2.0,2.2,1.4,2.2,2.3,1.0,
#' 2.2,3.8,1.5,2.2,2.0,1.6)
#' Ascorbic <-c(51,65,54,55,52,59,45,41,66,42,51,54,53,41,45,50,45,49,50,51,49,
#' 52,45,55,56,61,49,49,42,68,58,52,78,55,70,75,67,57,70,61,58,84,67,47,71,68,
#' 56,72,58,72,62,63,63,68,56,54,66,72,60,72)
#' trt<-paste(LineNumber,PlantingDate,sep="-")
#' output<-reg.homog(trt,HeadWt,Ascorbic)
#' 
reg.homog <-
function(trt,x,y) {
sumx<-function(x)(nrow(x)-1)*var(x)
datos<-data.frame(trt,x,y)
sumxy<-by(datos[,-1],trt,function(x) sumx(x))
t.vec<-as.character(unique(datos$trt))
r.total <-nrow(datos)
n.trt<-length(t.vec)
sx<-0; sy<-0; sxy<-0;residual<-0
for ( i in 1:n.trt) {
a1<-data.frame(sumxy[t.vec[i]])
a2<-as.matrix(a1)
residual <- residual+a2[2,2]-a2[1,2]^2/ a2[1,1]
sx<- sx + a2[1,1]
sy<- sy + a2[2,2]
sxy<- sxy + a2[1,2]
}
# suma de las regresiones
B<-sy-sxy^2/sx 
# suma de los residuales
A<- residual
# diferencia de homogeneidad
diff<-B-A
# Prueba de la homogenidad de regresiones
gl.trt <- n.trt-1
gl.r <- r.total - 2* n.trt
f.cal <- ( (B-A)/gl.trt ) /  ( A/gl.r)
p.value <- 1- pf( f.cal, gl.trt, gl.r)
resp<-"homogeneity of regressions exists"
if(p.value <= 0.05 ) resp<-"homogeneity of regressions does not exists"
# Imprime resultados
cat("\nTest of Homogeneity of regressions\n\n")
cat("Total of simple regressions: ", n.trt ,"\n")
cat("Total of residual          : ", A, "\n")
cat("Difference for homogeneity : ", B-A, "\n\n")
cat("D.f. for the homogeneity   : ", gl.trt,"\n")
cat("Residual degrees of freedom: ", gl.r,"\n")
cat("F calculated value         : ", f.cal,"\n")
cat("P.value                    : ", p.value,"\n\n")
cat("Criterion                  : ", resp,"\n\n")
output<-list(regressions=n.trt,residual=A,Difference=B,DF.homgeneity=gl.trt,DF.Residual=gl.r,F.value=f.cal,P.value=p.value,Criterion=resp)
return(output)
}
