#' Data for Friedman test
#' 
#' Twelve homeowners are selected randomly to participate in an experiment with
#' a plant nursery. Each homeowner is asked to select four fairly identical
#' areas in his yard and to plant four different types of grasses, one in each
#' area.
#' 
#' Each of the 12 blocks consists of four fairly identical plots of land, each
#' receiving care of approximately the same degree of skill because the four
#' plots are presumably cared for by the same homeowern.
#' 
#' @name grass
#' @docType data
#' @format A data frame with 48 observations on the following 3 variables.
#' \describe{ \item{list("judge")}{a numeric vector} \item{list("trt")}{a
#' factor with levels \code{t1} \code{t2} \code{t3} \code{t4}}
#' \item{list("evaluation")}{a numeric vector} }
#' @references Practical Nonparametrics Statistics. W.J. Conover, 1999
#' @source Book: Practical Nonparametrics Statistics, pag 372.
#' @keywords datasets
#' @examples
#' 
#' data(grass)
#' str(grass)
#' 
NULL