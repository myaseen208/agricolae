#' Data of cotton
#' 
#' Data of cotton collected in experiments of two localities in Lima and Pisco,
#' Peru.
#' 
#' 
#' @name cotton
#' @docType data
#' @format A data frame with 96 observations on the following 5 variables.
#' \describe{ \item{list("site")}{a factor with levels \code{Lima}
#' \code{Pisco}} \item{list("block")}{a factor with levels \code{I} \code{II}
#' \code{III} \code{IV} \code{V} \code{VI}} \item{list("lineage")}{a numeric
#' vector} \item{list("epoca")}{a numeric vector} \item{list("yield")}{a
#' numeric vector} }
#' @references Book spanish: Metodos estadisticos para la investigacion.
#' Autor: Calzada Benza Universidad Nacional Agraria - La Molina - Peru.
#' @source Book spanish: Metodos estadisticos para la investigacion.  Autor:
#' Calzada Benza Universidad Nacional Agraria - La Molina - Peru..
#' @keywords datasets
#' @examples
#' 
#' library(agricolae)
#' data(cotton)
#' str(cotton)
#' 
NULL