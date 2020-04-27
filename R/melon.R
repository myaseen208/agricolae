#' Data of yield of melon in a Latin square experiment
#' 
#' An irrigation system evaluation by exudation using four varieties of melon,
#' under modality of sowing, SIMPLE ROW.  The goal is to analyze the behavior
#' of three hybrid melon varieties and one standard.
#' 
#' Varieties: Hibrido Mission (V1); Hibrido Mark (V2); Hibrido Topfligth (V3);
#' Hibrido Hales Best Jumbo (V4).
#' 
#' @name melon
#' @docType data
#' @format A data frame with 16 observations on the following 4 variables.
#' \describe{ \item{list("row")}{a numeric vector} \item{list("col")}{a numeric
#' vector} \item{list("variety")}{a factor with levels \code{V1} \code{V2}
#' \code{V3} \code{V4}} \item{list("yield")}{a numeric vector} }
#' @references Universidad Nacional Agraria la molina.
#' @source Tesis. "Evaluacion del sistema de riego por exudacion utilizando
#' cuatro variedades de melon, bajo modalidad de siembra, SIMPLE HILERA".
#' Alberto Angeles L.  Universidad Agraria la Molina - Lima Peru.
#' @keywords datasets
#' @examples
#' 
#' library(agricolae)
#' data(melon)
#' str(melon)
#' 
NULL