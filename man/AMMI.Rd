\name{AMMI}
\alias{AMMI}
\title{ AMMI Analysis}
\description{
Additive Main Effects and Multiplicative Interaction Models (AMMI) are widely used 
to analyze main effects and genotype by environment (GEN, ENV) interactions in 
multilocation variety trials. Furthermore, this function generates data to biplot, triplot 
graphs and analysis.
}
\usage{
AMMI(ENV, GEN, REP, Y, MSE = 0,console=FALSE,PC=FALSE)
}

\arguments{
  \item{ENV}{ Environment }
  \item{GEN}{ Genotype }
  \item{REP}{ Replication }
  \item{Y}{ Response }
  \item{MSE}{ Mean Square Error }
  \item{console}{ ouput TRUE or FALSE }
  \item{PC}{ Principal components ouput TRUE or FALSE }
}
\details{
  additional graphics see help(plot.AMMI).
}
\value{
  \item{ANOVA}{analysis of variance general}
  \item{genXenv}{class by, genopyte and environment}
  \item{analysis}{analysis of variance principal components}
  \item{means}{average genotype and environment}
  \item{biplot}{data to produce graphics}
  \item{PC}{ class princomp }
}
\references{ 
Crossa, J. 1990. Statistical analysis of multilocation trials.
Advances in Agronomy 44:55-85
 }
\author{ F. de Mendiburu }

\seealso{\code{\link{lineXtester}},\code{\link{plot.AMMI}} }
\examples{
# Full replications
library(agricolae)
# Example 1
data(plrv)
model<- with(plrv,AMMI(Locality, Genotype, Rep, Yield, console=FALSE))
model$ANOVA
# see help(plot.AMMI)
# biplot
plot(model)
# triplot PC 1,2,3 
plot(model, type=2, number=TRUE)
# biplot PC1 vs Yield 
plot(model, first=0,second=1, number=TRUE)
# Example 2
data(CIC)
data1<-CIC$comas[,c(1,6,7,17,18)]
data2<-CIC$oxapampa[,c(1,6,7,19,20)]
cic <- rbind(data1,data2)
model<-with(cic,AMMI(Locality, Genotype, Rep, relative))
model$ANOVA
plot(model,0,1,angle=20,ecol="brown")
# Example 3
# Only means. Mean square error is well-known.
data(sinRepAmmi)
REP <- 3
MSerror <- 93.24224
#startgraph
model<-with(sinRepAmmi,AMMI(ENV, GEN, REP, YLD, MSerror,PC=TRUE))
# print anova
print(model$ANOVA,na.print = "")
# Biplot with the one restored observed.
plot(model,0,1,type=1)
# with principal components model$PC is class "princomp" 
pc<- model$PC
pc$loadings
summary(pc)
biplot(pc)
# Principal components by means of the covariance similar AMMI
# It is to compare results with AMMI
cova<-cov(model$genXenv)
values<-eigen(cova)
total<-sum(values$values)
round(values$values*100/total,2)
# AMMI: 64.81 18.58 13.50  3.11  0.00
}
\keyword{ models }

