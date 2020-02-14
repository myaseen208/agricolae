\name{heterosis}
\alias{heterosis}
\docType{data}
\title{ Data of potato, Heterosis}
\description{
  Determination of heterosis, general combining ability (GCA) and 
  specific combining ability in tuber dry matter, reducing sugars and 
  agronomic characteristics in TPS families.
}
\usage{data(heterosis)}
\format{
  A data frame with 216 observations on the following 11 variables.
  \describe{
    \item{\code{Place}}{1: La Molina, 2=Huancayo}
    \item{\code{Replication}}{a numeric vector}
    \item{\code{Treatment}}{a numeric vector}
    \item{\code{Factor}}{a factor with levels \code{Control} \code{progenie} \code{progenitor} \code{testigo}}
    \item{\code{Female}}{a factor with levels \code{Achirana} \code{LT-8} \code{MF-I} \code{MF-II} \code{Serrana} \code{TPS-2} \code{TPS-25} \code{TPS-7}}
    \item{\code{Male}}{a factor with levels \code{TPS-13} \code{TPS-67} \code{TS-15}}
    \item{\code{v1}}{Yield (Kg/plant)}
    \item{\code{v2}}{Reducing sugars (scale):1 low and 5=High}
    \item{\code{v3}}{Tuber dry matter (percentage)}
    \item{\code{v4}}{Tuber number/plant}
    \item{\code{v5}}{Average tuber weight (g)}
  }
}
\details{
  The study was conducted in 3 environments, La Molina-PERU to 240 masl. during 
  autumn-winter and spring, and in Huancayo-PERU 3180 masl., during summer. 
  The experimental material consisted of 24 families half brother in the form 
  of tubers derived from TPS, obtained crossing between 8 female and 3 male parents. 
  Design used was randomized complete block with three repetitions. 
  The experimental unit was 30 plants in two rows at a distance of 30cm 
  between plants and 90 cm between rows. Variables evaluated were Yield, Tubers number, 
  Dry matter and content and reducing sugars. The analysis was conducted 
  line x tester. The control variety was Desiree.
}
\source{
 International Potato Center(CIP). Lima-Peru. Data Kindly provided by of Rolando Cabello. 
}
\references{
Tesis "Heterosis, habilidad combinatoria general y especifica para materia seca, 
azucares reductores y caracteres agronomicos en familias de tuberculos provenientes 
de semilla sexual de papa. Magister Scientiae Rodolfo Valdivia Lorente. Universidad 
Nacional Agraria La molina-Lima Peru, Escuela de Post Grado, Mejoramiento genetico
de plantas, 2004". Poster: Congreso de la Sociedad Peruana de Genetica - Peru, 2008.  
}
\examples{
library(agricolae)
data(heterosis)
str(heterosis)
site1<-subset(heterosis,heterosis[,1]==1)
site2<-subset(heterosis,heterosis[,1]==2)
site3<-subset(heterosis,heterosis[,1]==3)
model1<-with(site1,lineXtester(Replication, Female, Male, v1))
DFe <- df.residual(model1)
CMe <- deviance(model1)/DFe
test1 <- with(site1,HSD.test(v1, Factor,DFe,CMe))
test2 <- with(site1,HSD.test(v1, Treatment,DFe,CMe))
model22<-with(site2,lineXtester(Replication, Female, Male, v3))
model3<-with(site3,lineXtester(Replication, Female, Male, v4))
}
\keyword{datasets}
