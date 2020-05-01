#' @title Data of yield of melon in a Latin square experiment
#' @name   melon
#' @docType data
#' @keywords datasets
#' @usage data(melon)
#' @description
#' An irrigation system evaluation by exudation using four varieties of melon,
#' under modality of sowing, SIMPLE ROW.  The goal is to analyze the behavior
#' of three hybrid melon varieties and one standard.
#' 
#' Varieties: Hibrido Mission (V1); Hibrido Mark (V2); Hibrido Topfligth (V3);
#' Hibrido Hales Best Jumbo (V4).
#' 
#' @format 
#' A \code{data.frame} with 16 observations on the following 4 variables.
#'
#'  @details
#'         \itemize{
#'         \item{\strong{row}} row
#'         \item{\strong{col}} col
#'         \item{\strong{variety}} a factor with levels V1, V2, V3, andV4
#'         \item{\strong{yield}} yield
#'         }
#'         
#' @references 
#' Universidad Nacional Agraria la molina.
#' 
#' @source 
#' Thesis. "Evaluacion del sistema de riego por exudacion utilizando
#' cuatro variedades de melon, bajo modalidad de siembra, SIMPLE HILERA".
#' Alberto Angeles L.  Universidad Agraria la Molina - Lima Peru.
#' 
#' @keywords datasets
#' @examples
#' 
#' library(agricolae)
#' data(melon)
#' str(melon)
#' 
NULL