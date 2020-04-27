#' Simulation of the linear model under normality
#' 
#' This process consists of validating the variance analysis results using a
#' simulation process of the experiment.  The validation consists of comparing
#' the calculated values of each source of variation of the simulated data with
#' respect to the calculated values of the original data. If in more than 50
#' percent of the cases they are higher than the real one, then it is
#' considered favorable and the probability reported by the ANOVA is accepted,
#' since the P-Value is the probability of (F > F.value).
#' 
#' 
#' @param model Model in R
#' @param file Data for the study of the model
#' @param categorical position of the columns of the data that correspond to
#' categorical variables
#' @param k Number of simulations
#' @param console logical, print output
#' @return \item{model}{ ouput linear model, lm} \item{simulation}{ anova
#' simulation}
#' @author Felipe de Mendiburu
#' @seealso \code{\link{resampling.model} }
#' @keywords models
#' @importFrom stats as.formula rnorm
#' @export
#' @examples
#' 
#' library(agricolae)
#' #example 1
#' data(clay)
#' model<-"ralstonia ~ days"
#' simulation.model(model,clay,k=15,console=TRUE)
#' #example 2
#' data(sweetpotato)
#' model<-"yield~virus"
#' simulation.model(model,sweetpotato,categorical=1,k=15,console=TRUE)
#' #example 3
#' data(Glycoalkaloids)
#' model<-"HPLC ~ spectrophotometer"
#' simulation.model(model,Glycoalkaloids,k=15,console=TRUE)
#' #example 4
#' data(potato)
#' model<-"cutting~date+variety"
#' simulation.model(model,potato,categorical=c(1,2,3),k=15,console=TRUE)
#' 
#' 
simulation.model <-
function(model,file,categorical=NULL,k,console=FALSE) {
modelo<-model
parte<-strsplit(model,"~")[[1]]
model<-as.formula(model)
posicion<-0
if(length(categorical)>0){
posicion<-categorical
n<-length(posicion)
for( i in 1:n) {
pos<-posicion[i]
file[,pos]<-as.factor(file[,pos])
}
}
ecuacion<-lm(model,data=file)
xx<-data.frame(anova(ecuacion))
xx[,2]<-xx[,4]
fc<-xx[,4]
names(xx)<-c("Df","F value", "% Acceptance", "% Rejection", "Criterion")
gl<-anova(ecuacion)$Df
gk<-length(gl)-1
xx<-xx[-(gk+1),]
predicho<-predict(ecuacion)
m<-length(predicho)
sd.model<-sqrt(deviance(ecuacion)/gl[gk+1])
f<-rep(0,k)
cuenta <- rep(0,gk)
model <- paste("y","~",parte[2])
model<-as.formula(model)
for(i in 1:k){
errores<-rnorm(m,0,sd.model)
# Simula nuevos datos experimentales
y<-predicho+errores
simula<-lm(model,data=file)
for (j in 1:gk){
f[j]<-anova(simula)[j,4]
if(f[j] >= fc[j])cuenta[j]<-cuenta[j]+1
}
}
for( j in 1:gk){
xx[j,3]<-cuenta[j]*100/k
xx[j,4]<-100-xx[j,3]
if(xx[j,3]>50) xx[j,5]<- "acceptable"
if(xx[j,3]==50) xx[j,5]<- "uncertain"
if(xx[j,3]<50) xx[j,5]<- "nonacceptable"
}
if(console){
cat("\nSimulation of experiments\nUnder the normality assumption\n")
cat(rep("-",16),"\n")
cat("Proposed model:",modelo,"\n")
print(anova(ecuacion))
cat("---\n")
cat("Validation of the analysis of variancia for the proposed model\n")
cat("Simulations:",k,"\n\n")
print(xx)
cat("---\n\n")
}
out<-list(model=ecuacion, simulation=xx)
invisible(out)
}
