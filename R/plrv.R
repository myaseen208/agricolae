#' Data clones from the PLRV population
#' 
#' Six environments: Ayacucho, La Molina 02, San Ramon 02, Huancayo, La Molina
#' 03, San Ramon 03.
#' 
#' 
#' @name plrv
#' @docType data
#' @format A data frame with 504 observations on the following 6 variables.
#' \describe{ \item{list("Genotype")}{a factor with levels \code{102.18}
#' \code{104.22} \code{121.31} \code{141.28} \code{157.26} \code{163.9}
#' \code{221.19} \code{233.11} \code{235.6} \code{241.2} \code{255.7}
#' \code{314.12} \code{317.6} \code{319.20} \code{320.16} \code{342.15}
#' \code{346.2} \code{351.26} \code{364.21} \code{402.7} \code{405.2}
#' \code{406.12} \code{427.7} \code{450.3} \code{506.2} \code{Canchan}
#' \code{Desiree} \code{Unica}} \item{list("Locality")}{a factor with levels
#' \code{Ayac} \code{Hyo-02} \code{LM-02} \code{LM-03} \code{SR-02}
#' \code{SR-03}} \item{list("Rep")}{a numeric vector}
#' \item{list("WeightPlant")}{a numeric vector} \item{list("WeightPlot")}{a
#' numeric vector} \item{list("Yield")}{a numeric vector} }
#' @references International Potato Center Lima-Peru
#' @source International Potato Center Lima-Peru
#' @keywords datasets
#' @examples
#' 
#' library(agricolae)
#' data(plrv)
#' str(plrv)
#' 
NULL