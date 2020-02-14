\name{soil}
\alias{soil}
\docType{data}
\title{ Data of soil analysis for 13 localities}
\description{
  We analyzed the physical and chemical properties of different soils, as full 
  characterization of soil and special analysis of micro-elements. These analyses 
  were conducted in the laboratory analysis of soils, plants, water and fertilizers 
  in the La Molina National Agrarian University (UNALM). To which the different soil 
  samples were dried to the environment, screened (mesh 0.5xo, 5 mm) and sterilized by 
  steam 4 to 5 hours with a Lindinger Steam aerator SA150 and SA700, with the possible 
  aim of eliminating bacteria saprophytic or antagonists to prevent the growth of bacteria 
  (R.solanacearum).
  }
\usage{data(soil)}
\format{
  A data frame with 13 observations on the following 23 variables.
  \describe{
    \item{\code{place}}{a factor with levels \code{Chmar} \code{Chz} \code{Cnt1} \code{Cnt2} \code{Cnt3} \code{Hco1} \code{Hco2} \code{Hco3} \code{Hyo1} \code{Hyo2} \code{Namora} \code{SR1} \code{SR2}}
    \item{\code{pH}}{a numeric vector}
    \item{\code{EC}}{a numeric vector, electrical conductivity}
    \item{\code{CaCO3}}{a numeric vector}
    \item{\code{MO}}{a numeric vector}
    \item{\code{CIC}}{a numeric vector}
    \item{\code{P}}{a numeric vector}
    \item{\code{K}}{a numeric vector}
    \item{\code{sand}}{a numeric vector}
    \item{\code{slime}}{a numeric vector}
    \item{\code{clay}}{a numeric vector}
    \item{\code{Ca}}{a numeric vector}
    \item{\code{Mg}}{a numeric vector}
    \item{\code{K2}}{a numeric vector}
    \item{\code{Na}}{a numeric vector}
    \item{\code{Al_H}}{a numeric vector}
    \item{\code{K_Mg}}{a numeric vector}
    \item{\code{Ca_Mg}}{a numeric vector}
    \item{\code{B}}{a numeric vector}
    \item{\code{Cu}}{a numeric vector}
    \item{\code{Fe}}{a numeric vector}
    \item{\code{Mn}}{a numeric vector}
    \item{\code{Zn}}{a numeric vector}
  }
}
\details{
Cnt1= Canete, Cnt2=Valle Dulce(Canete), Cnt3=Valle Grande(Canete), Chz=Obraje-Carhuaz(Ancash),
Chmar=Chucmar-Chota(Huanuco, Hco1= Mayobamba-Chinchao(Huanuco), Hco2=Nueva Independencia-Chinchao(Huanuco), 
Hco3=San Marcos-Umari(Huanuco), Hyo1=La Victoria-Huancayo(Junin), Hyo1=El Tambo-Huancayo(Junin), 
Namora=Namora(Cajamarca), SR1= El Milagro-San Ramon(Junin), Sr2=La Chinchana-San Ramon(Junin).
  }
\source{
  Experimental field, 2004. Data Kindly provided by Dr. Sylvie Priou, Liliam Gutarra and Pedro Aley.
}
\references{
  International Potato Center - Lima, PERU.
}
\examples{
library(agricolae)
data(soil)
str(soil)
}
\keyword{datasets}
