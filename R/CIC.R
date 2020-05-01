#' @title Data for late blight of potatoes
#' @name   CIC
#' @docType data
#' @keywords datasets
#' @usage data(CIC)
#' @description
#' A study of Phytophthora infestans in the potato plant in the localities of
#' Comas and Oxapampa in Peru, 2005.
#' 
#' comas: temperature=59.9 Fahrenheit, relative humidity=83.3 oxapampa:
#' temperature=64.8 Fahrenheit, relative humidity=86.2 AUDPC and relative see
#' function audpc(). help(audpc) Exx: Evaluation in percentaje, xx is days.
#' ORD1, ORD2, SBLK and row are references location of the plot in the field.
#'
#' @format 
#' An object of class \code{list} with two elements: comas and oxapampa
#' 
#'  @details
#'         \itemize{
#'         \item{\strong{Locality}} Locality: Comas, Oxapampa
#'         \item{\strong{Genotype}} Genotype
#'         \item{\strong{Rep}} Rep
#'         \item{\strong{E9}} infestans percentage to 9 days
#'         \item{\strong{AUDPC}} the area under the disease-progress curve
#'         \item{\strong{Relative}}     relative area
#'         }
#'          
#' 
#' @references 
#' International Potato Center. CIP - Lima Peru.
#' 
#' @source 
#' Experimental field, 2004-2005. Data Kindly provided by Matilde Orrillo.
#' 
#' @examples
#' 
#' library(agricolae)
#' data(CIC)
#' CIC$comas
#' CIC$oxapampa
#' 
NULL