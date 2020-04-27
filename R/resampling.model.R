#' Resampling for linear models
#' 
#' This process consists of finding the values of P-value by means of a
#' re-sampling (permutation) process along with the values obtained by variance
#' analysis.
#' 
#' 
#' @param model model in R
#' @param data data for the study of the model
#' @param k number of re-samplings
#' @param console logical, print output
#' @return Model solution with resampling.
#' @author Felipe de Mendiburu
#' @seealso \code{\link{simulation.model} }
#' @references Efron, B., Tibshirani, R. (1993) An Introduction to the
#' Boostrap. Chapman and Hall/CRC Phillip I. Good, (2001) Resampling Methods.
#' Birkhauser. Boston . Basel . Berlin
#' @keywords multivariate
#' @importFrom stats as.formula
#' @export
#' @examples
#' 
#' #example 1 Simple linear regression
#' library(agricolae)
#' data(clay)
#' model<-"ralstonia ~ days"
#' analysis<-resampling.model(model,clay,k=2,console=TRUE)
#' 
#' #example 2 Analysis of variance: RCD
#' data(sweetpotato)
#' model<-"yield~virus"
#' analysis<-resampling.model(model,sweetpotato,k=2,console=TRUE)
#' 
#' #example 3 Simple linear regression
#' data(Glycoalkaloids)
#' model<-"HPLC ~ spectrophotometer"
#' analysis<-resampling.model(model,Glycoalkaloids,k=2,console=TRUE)
#' 
#' #example 4 Factorial in RCD
#' 
#' data(potato)
#' potato[,1]<-as.factor(potato[,1])
#' potato[,2]<-as.factor(potato[,2])
#' model<-"cutting~variety + date + variety:date"
#' analysis<-resampling.model(model,potato,k=2,console=TRUE)
#' 
#' 
resampling.model <-
function(model,data,k,console=FALSE) {
modelo<-model
parte<-strsplit(model,"~")[[1]]
model<-as.formula(model)
ecuacion<-lm(model,data=data)
xx<-data.frame(anova(ecuacion),NA)
yy<-ecuacion$model[[1]]
fc<-xx[,4]
names(xx)<-c("Df","Sum Sq","Mean Sq","F value","Pr(>F)","Resampling")
m<-nrow(data)
gk<-nrow(xx)-1
f<-rep(0,gk)
cuenta <- rep(0,gk)
# Start Resampling
model <- paste("y","~",parte[2])
model<-as.formula(model)
for(i in 1:k){
y<-sample(yy,m)
resample<-lm(model,data=data)
for (j in 1:gk){
f[j]<-anova(resample)[j,4]
if(f[j] >= fc[j])cuenta[j]<-cuenta[j]+1
}
}
# finish resampling

for( j in 1:gk){
xx[j,6]<-cuenta[j]/k
}
if(console){
cat("\nResampling of the experiments\n")
cat(rep("-",14),"\n")
cat("Proposed model:",modelo,"\n")
cat("---\n")
cat("Resampling of the analysis of variancia for the proposed model\n")
cat("Determination of the P-Value by Resampling\n")
cat("Samples:",k,"\n\n")
xx<-as.matrix(xx)
print(xx,na.print="")
cat("---\n\n")
}
out<-list(model=resample, solution=xx,acum=cuenta,samples=k)
invisible(out)
}
