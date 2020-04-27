#' Data evaluation of the disease overtime
#' 
#' Three evaluations over time and the potato yield when applying several
#' treatments.
#' 
#' 
#' @name disease
#' @docType data
#' @format A data frame with 21 observations on the following 7 variables.
#' \describe{ \item{list("plots")}{a numeric vector} \item{list("rep")}{a
#' numeric vector} \item{list("trt")}{a factor with levels \code{T0} \code{T1}
#' \code{T2} \code{T3} \code{T4} \code{T5} \code{T6}} \item{list("E2")}{a
#' numeric vector} \item{list("E5")}{a numeric vector} \item{list("E7")}{a
#' numeric vector} \item{list("yield")}{a numeric vector} }
#' @references International Potato Center. CIP - Lima Peru.
#' @source Experimental data.
#' @keywords datasets
#' @examples
#' 
#' library(agricolae)
#' data(disease)
#' str(disease)
#' 
NULL