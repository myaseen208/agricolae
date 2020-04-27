#' Data for AMMI without repetition
#' 
#' Data frame for AMMI analysis with 50 genotypes in 5 environments.
#' 
#' 
#' @name sinRepAmmi
#' @docType data
#' @format A data frame with 250 observations on the following 3 variables.
#' \describe{ \item{list("ENV")}{a factor with levels \code{A1} \code{A2}
#' \code{A3} \code{A4} \code{A5}} \item{list("GEN")}{a numeric vector}
#' \item{list("YLD")}{a numeric vector} }
#' @references International Potato Center - Lima Peru.
#' @source Experimental data.
#' @keywords datasets
#' @examples
#' 
#' library(agricolae)
#' data(sinRepAmmi)
#' str(sinRepAmmi)
#' 
NULL