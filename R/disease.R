#' @title Data evaluation of the disease overtime
#' @name   disease
#' @docType data
#' @keywords datasets
#' @usage data(disease)
#' @description
#' Three evaluations over time and the potato yield when applying several
#' treatments.
#' 
#' 
#' @format A \code{data.frame} with 21 observations on the following 7 variables.
#' 
#' 
#'  @details
#'         \itemize{
#'         \item{\strong{plots}} plots
#'         \item{\strong{rep}} rep
#'         \item{\strong{trt}} a factor with levels \code{T0}, \code{T1}, \code{T2}, \code{T3}, \code{T4}, \code{T5}, \code{T6}
#'         \item{\strong{E2}} E2
#'         \item{\strong{E5}} E5
#'         \item{\strong{E7}} E7
#'         \item{\strong{yield}} yield
#'         }
#' 
#' @references 
#' International Potato Center. CIP - Lima Peru.
#' 
#' @source 
#' Experimental data.
#' 
#' @keywords datasets
#' @examples
#' 
#' library(agricolae)
#' data(disease)
#' str(disease)
#' 
NULL