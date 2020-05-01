#' @title Data of soil analysis for 13 localities
#' @name   soil
#' @docType data
#' @keywords datasets
#' @usage data(soil)
#' @description
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
#' @format 
#' A \code{data.frame} with 13 observations on the following 23 variables.
#'  
#'  @details
#'         \itemize{
#'         \item{\strong{place}} a factor with levels 
#'               Chmar, Chz, Cnt1, Cnt2, Cnt3, 
#'               Hco1, Hco2, Hco3, Hyo1, Hyo2, Namora, SR1, and SR2
#'         \item{\strong{pH}} pH
#'         \item{\strong{EC}} electrical conductivity
#'         \item{\strong{CaCO3}} CaCO3
#'         \item{\strong{MO}} MO 
#'         \item{\strong{CIC}} CIC
#'         \item{\strong{P}} P
#'         \item{\strong{K}} K
#'         \item{\strong{sand}} sand
#'         \item{\strong{slime}} slime
#'         \item{\strong{clay}} clay
#'         \item{\strong{Ca}} Ca
#'         \item{\strong{Mg}} Mg
#'         \item{\strong{K2}} K2
#'         \item{\strong{Na}} Na
#'         \item{\strong{Al_H}} Al_H
#'         \item{\strong{K_Mg}} K_Mg
#'         \item{\strong{Ca_Mg}} Ca_Mg
#'         \item{\strong{B}} B
#'         \item{\strong{Cu}} Cu
#'         \item{\strong{Fe}} Fe
#'         \item{\strong{Mn}} Mn
#'         \item{\strong{Zn}} Zn
#'         }
#'         
#' @references 
#' International Potato Center - Lima, PERU.
#' 
#' @source 
#' Experimental field, 2004. Data Kindly provided by Dr. Sylvie Priou,
#' Liliam Gutarra and Pedro Aley.
#' 
#' @keywords datasets
#' @examples
#' 
#' library(agricolae)
#' data(soil)
#' str(soil)
#' 
NULL