#' Data of frijol
#' 
#' Data of frijol under 4 technologies for the homogeneity of regression study.
#' Yield of Frijol in kg/ha in clean and dry grain.
#' 
#' Tecnnologies: 20-40-20 kg/ha.  N. P2O5 and K2O + 2 t/ha of gallinaza.
#' 40-80-40 kg/ha.  N. P2O5 and K2O + 2 t/ha of gallinaza.  60-120-60 kg/ha. N.
#' P2O5 and K2O + 2 t/ha of gallinaza.  40-80-40 kg/ha.  N. P2O5 and K2O + 4
#' t/ha of gallinaza.
#' 
#' 
#' @name frijol
#' @docType data
#' @format A data frame with 84 observations on the following 3 variables.
#' \describe{ \item{list("technology")}{a factor with levels \code{a} \code{b}
#' \code{c} \code{d}} \item{list("production")}{a numeric vector}
#' \item{list("index")}{a numeric vector} }
#' @references Oriente antioqueno (1972) (ICA.- Orlando Martinez W.) Colombia.
#' @keywords datasets
#' @examples
#' 
#' library(agricolae)
#' data(frijol)
#' str(frijol)
#' 
NULL