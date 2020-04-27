#' Data of soil analysis for 13 localities
#' 
#' We analyzed the physical and chemical properties of different soils, as full
#' characterization of soil and special analysis of micro-elements. These
#' analyses were conducted in the laboratory analysis of soils, plants, water
#' and fertilizers in the La Molina National Agrarian University (UNALM). To
#' which the different soil samples were dried to the environment, screened
#' (mesh 0.5xo, 5 mm) and sterilized by steam 4 to 5 hours with a Lindinger
#' Steam aerator SA150 and SA700, with the possible aim of eliminating bacteria
#' saprophytic or antagonists to prevent the growth of bacteria
#' (R.solanacearum).
#' 
#' Cnt1= Canete, Cnt2=Valle Dulce(Canete), Cnt3=Valle Grande(Canete),
#' Chz=Obraje-Carhuaz(Ancash), Chmar=Chucmar-Chota(Huanuco, Hco1=
#' Mayobamba-Chinchao(Huanuco), Hco2=Nueva Independencia-Chinchao(Huanuco),
#' Hco3=San Marcos-Umari(Huanuco), Hyo1=La Victoria-Huancayo(Junin), Hyo1=El
#' Tambo-Huancayo(Junin), Namora=Namora(Cajamarca), SR1= El Milagro-San
#' Ramon(Junin), Sr2=La Chinchana-San Ramon(Junin).
#' 
#' @name soil
#' @docType data
#' @format A data frame with 13 observations on the following 23 variables.
#' \describe{ \item{list("place")}{a factor with levels \code{Chmar} \code{Chz}
#' \code{Cnt1} \code{Cnt2} \code{Cnt3} \code{Hco1} \code{Hco2} \code{Hco3}
#' \code{Hyo1} \code{Hyo2} \code{Namora} \code{SR1} \code{SR2}}
#' \item{list("pH")}{a numeric vector} \item{list("EC")}{a numeric vector,
#' electrical conductivity} \item{list("CaCO3")}{a numeric vector}
#' \item{list("MO")}{a numeric vector} \item{list("CIC")}{a numeric vector}
#' \item{list("P")}{a numeric vector} \item{list("K")}{a numeric vector}
#' \item{list("sand")}{a numeric vector} \item{list("slime")}{a numeric vector}
#' \item{list("clay")}{a numeric vector} \item{list("Ca")}{a numeric vector}
#' \item{list("Mg")}{a numeric vector} \item{list("K2")}{a numeric vector}
#' \item{list("Na")}{a numeric vector} \item{list("Al_H")}{a numeric vector}
#' \item{list("K_Mg")}{a numeric vector} \item{list("Ca_Mg")}{a numeric vector}
#' \item{list("B")}{a numeric vector} \item{list("Cu")}{a numeric vector}
#' \item{list("Fe")}{a numeric vector} \item{list("Mn")}{a numeric vector}
#' \item{list("Zn")}{a numeric vector} }
#' @references International Potato Center - Lima, PERU.
#' @source Experimental field, 2004. Data Kindly provided by Dr. Sylvie Priou,
#' Liliam Gutarra and Pedro Aley.
#' @keywords datasets
#' @examples
#' 
#' library(agricolae)
#' data(soil)
#' str(soil)
#' 
NULL