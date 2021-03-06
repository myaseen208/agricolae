#' @title Data Yacon
#' @name   yacon
#' @docType data
#' @keywords datasets
#' @usage data(yacon)
#' @description
#' The yacon (Smallanthus sonchifolius) is a plant native to the Andes,
#' considered a traditional crop in Peru and natural source of FOS, which is a
#' type of carbohydrate that can not be digested by the and the human body that
#' have joined several beneficial properties in health, such as improve the
#' absorption of calcium, reducing the level of triglycerides and cholesterol
#' and stimulate better gastrointestinal function.
#' 
#' Proportion or fraction of the plant that is used (seeds, fruit, root) on dry
#' basis. Part usable in a proportion of total mass dissected. Plant of frijol,
#' weight = 100g and frijol = 50g then, IH = 50/100 = 0.5 or 50 percentaje.
#' Degrees Brix is a measurement of the mass ratio of dissolved sugar to water
#' in a liquid.
#' 
#' @format 
#' A \code{data.frame} with 432 observations on the following 19 variables.
#' 
#' @details
#'         \itemize{
#'         \item{\strong{locality}} a factor with levels, Cajamarca, Lima, Oxapampa in PERU
#'         \item{\strong{site}} site
#'         \item{\strong{dose}} a factor with levels F0, F150, and F80
#'         \item{\strong{entry}} a factor with levels AKW5075, AMM5136, AMM5150, AMM5163, 
#'                              ARB5125, CLLUNC118, P1385, and SAL136
#'         \item{\strong{replication}} replication
#'         \item{\strong{height}} plant height, centimeters
#'         \item{\strong{stalks}} number of stalks
#'         \item{\strong{wfr}} weight of fresh roots, grams
#'         \item{\strong{wff}} weight of fresh foliage, grams
#'         \item{\strong{wfk}} weight fresh kroner, grams
#'         \item{\strong{roots}} matter of dried roots, grams
#'         \item{\strong{FOS}} fructo-oligosaccharides, percentage
#'         \item{\strong{glucose}} glucose
#'         \item{\strong{fructose}} fructose
#'         \item{\strong{sucrose}} sucrose
#'         \item{\strong{brix}} brix
#'         \item{\strong{foliage}} foliage
#'         \item{\strong{dry}} dry
#'         \item{\strong{IH}} IH
#'         }
#'         
#' 
#' @references 
#' International Potato Center. CIP - Lima Peru.
#' 
#' @source 
#' CIP. Experimental field, 2003, Data Kindly provided by Ivan Manrique
#' and Carolina Tasso.
#' 
#' @keywords datasets
#' @examples
#' 
#' library(agricolae)
#' data(yacon)
#' str(yacon) 
#' 
NULL