#' @title Data clones from the PLRV population
#' @name   plrv
#' @docType data
#' @keywords datasets
#' @usage data(plrv)
#' @description
#' Six environments: Ayacucho, La Molina 02, San Ramon 02, Huancayo, La Molina
#' 03, San Ramon 03.
#' 
#' @format 
#' A \code{data.frame} with 504 observations on the following 6 variables.
#' 
#'  @details
#'         \itemize{
#'         \item{\strong{Genotype}} a factor with levels 
#'           102.18, 104.22, 121.31, 141.28, 157.26, 163.9,
#'           221.19, 233.11, 235.6, 241.2, 255.7, 314.12,
#'           317.6, 319.20, 320.16, 342.15, 346.2, 351.26,
#'           364.21, 402.7, 405.2, 406.12, 427.7, 450.3, 506.2,
#'           Canchan, Desiree, and Unica
#'         \item{\strong{Locality}} a factor with levels 
#'                     Ayac, Hyo-02, LM-02, LM-03, SR-02, and SR-03
#'         \item{\strong{Rep}} Rep
#'         \item{\strong{WeightPlant}} WeightPlant
#'         \item{\strong{WeightPlot}} WeightPlot
#'         \item{\strong{Yield}}     Yield
#'         }
#'         
#' 
#' @references 
#' International Potato Center Lima-Peru
#' 
#' @source 
#' International Potato Center Lima-Peru
#' 
#' @keywords datasets
#' @examples
#' 
#' library(agricolae)
#' data(plrv)
#' str(plrv)
#' 
NULL