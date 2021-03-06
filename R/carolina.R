#' @name    carolina
#' @aliases carolina
#' @title North Carolina Designs I, II and III
#' @description
#' Statistic analysis of the Carolina I, II and III genetic designs.
#' 
#' model = 1,2 and 3 is I, II and III see carolina1,2 and 3.
#' 
#' @param model Constant
#' @param data Data frame
#' 
#' @return \strong{model} model analysis (I, II or III) of caroline design
#' @return \strong{variance}  additive variance of male, female and male.female interaction.
#' 
#' @author
#' \enumerate{
#'          \item Felipe de Mendiburu (\email{fmendiburu@@lamolina.edu.pe})
#'          \item Muhammad Yaseen (\email{myaseen208@@gmail.com})
#'          }
#' 
#' @seealso 
#' \code{\link{DC}}
#' 
#' @references 
#' Biometrical Methods in Quantitative Genetic Analysis, Singh, Chaudhary. 1979
#' 
#' @keywords models
#' 
#' 
#' @importFrom stats anova
#' @export
#' 
#' @examples
#' 
#' 
#' library(agricolae)
#' data(DC)
#' carolina1 <- DC$carolina1
#' # str(carolina1)
#' output<-carolina(model=1,carolina1)
#' output[][-1]
#' 
#' carolina2 <- DC$carolina2
#' # str(carolina2)
#' majes<-subset(carolina2,carolina2[,1]==1)
#' majes<-majes[,c(2,5,4,3,6:8)]
#' output<-carolina(model=2,majes[,c(1:4,6)])
#' output[][-1]
#' 
#' carolina3 <- DC$carolina3
#' # str(carolina3)
#' output<-carolina(model=3,carolina3)
#' output[][-1]
#' 
carolina <-
function(model,data) {
if(model==1) {
# Observe la estructura de los datos carolina1
"set"<-as.factor(data[,1])
"male"<-as.factor(data[,2])
"female"<-as.factor(data[,3])
"progenie"<-as.factor(data[,4])
"replication"<-as.factor(data[,5])
y<-data[,6]
name.y <- names(data)[6]
#analisis
model<-lm(y ~ set+replication%in%set+male%in%set+female%in%male%in%set+replication%in%female%in%male%in%set)
cat("Response(y): ",name.y,"\n\n")
print(anova(model))
cat("\nCV:",cv.model(model), "\tMean:",mean(data[,6]),"\n" )
m<-length(levels(model$model$male))
f<-length(levels(model$model$female))
s<-length(levels(model$model$set))
r<-length(levels(model$model$replication))
n<-length(levels(progenie))
# Componentes de variancia
anva<-as.matrix(anova(model))
anva<-anva[,1:3]
var.m<- (anva["set:male","Mean Sq"] - anva["set:male:female","Mean Sq"])/(f*r*n)
var.f<- (anva["set:male:female","Mean Sq"] - anva["set:replication:male:female","Mean Sq"])/(n*r)
var.A<- 4*var.m
var.D<- 4*var.f-4*var.m
output<-list(model=model,var.m=var.m,var.f=var.f, var.A=var.A , var.D=var.D)
return(output)
}
if(model==2) {
# Observe la estructura de los datos carolina2
"set"<-as.factor(data[,1])
"male"<-as.factor(data[,2])
"female"<-as.factor(data[,3])
"replication"<-as.factor(data[,4])
y<-data[,5]
name.y <- names(data)[5]
# Analisis
model<-lm(y ~ set+replication%in%set+male%in%set+female%in%set+male:female%in%set)
cat("Response(y): ",name.y,"\n\n")
print(anova(model))
cat("\nCV:",cv.model(model), "\tMean:",mean(y),"\n" )
m<-length(levels(model$model$male))
f<-length(levels(model$model$female))
s<-length(levels(model$model$set))
r<-length(levels(model$model$replication))
# Componentes de variancia
anva<-as.matrix(anova(model))
anva<-anva[,1:3]
var.m<- (anva["set:male", "Mean Sq"] - anva["set:male:female","Mean Sq"])/(m*r)
var.f<- (anva["set:female","Mean Sq"] - anva["set:male:female","Mean Sq"])/(f*r)
var.mf<- (anva["set:male:female","Mean Sq"] - anva["Residuals","Mean Sq"])/r
var.Am<- 4*var.m
var.Af<- 4*var.f
var.D <- 4*var.mf
output<-list(model=model,var.m=var.m,var.f=var.f, var.mf=var.mf,var.Am=var.Am ,var.Af=var.Af, var.D=var.D)
return(output)
}

if(model==3) {
# Observe la estructura de los datos carolina3
"set"<-as.factor(data[,1])
"male"<-as.factor(data[,2])
"female"<-as.factor(data[,3])
"replication"<-as.factor(data[,4])
y<-data[,5]
name.y <- names(data)[5]
model<-lm(y~set + replication%in%set + female%in%set + male%in%set+ female:male%in%set)
cat("Response(y): ",name.y,"\n\n")
print(anova(model))
cat("\nCV:",cv.model(model), "\tMean:",mean(y),"\n" )
m<-length(levels(model$model$male))
f<-length(levels(model$model$female))
s<-length(levels(model$model$set))
r<-length(levels(model$model$replication))

# Componentes de variancia
anva<-as.matrix(anova(model))
anva<-anva[,1:3]
var.mi<- (anva[5,3] - anva["Residuals","Mean Sq"])/r
var.m<- (anva["set:male","Mean Sq"] - anva["Residuals","Mean Sq"])/(2*r)
var.A<- 4*var.m
var.D<- 2*var.mi
output<-list(model=model,var.mi=var.mi,var.m=var.m, var.A=var.A, var.D=var.D)
return(output)
}
}
