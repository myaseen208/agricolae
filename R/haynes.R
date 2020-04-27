#' Data of AUDPC for nonparametrical stability analysis
#' 
#' Published data. Haynes. Mean area under the disease progress curve (AUDPC)
#' for each of 16 potato clones evaluated at eight sites across the United
#' States in 1996
#' 
#' 
#' @name haynes
#' @docType data
#' @format A data frame with 16 observations on the following 9 variables.
#' \describe{ \item{list("clone")}{a factor with levels \code{A84118-3}
#' \code{AO80432-1} \code{AO84275-3} \code{AWN86514-2} \code{B0692-4}
#' \code{B0718-3} \code{B0749-2F} \code{B0767-2} \code{Bertita} \code{Bzura}
#' \code{C0083008-1} \code{Elba} \code{Greta} \code{Krantz} \code{Libertas}
#' \code{Stobrawa}} \item{list("FL")}{a numeric vector} \item{list("MI")}{a
#' numeric vector} \item{list("ME")}{a numeric vector} \item{list("MN")}{a
#' numeric vector} \item{list("ND")}{a numeric vector} \item{list("NY")}{a
#' numeric vector} \item{list("PA")}{a numeric vector} \item{list("WI")}{a
#' numeric vector} }
#' @references Haynes K G, Lambert D H, Christ B J, Weingartner D P, Douches D
#' S, Backlund J E, Fry W and Stevenson W. 1998. Phenotypic stability of
#' resistance to late blight in potato clones evaluated at eight sites in the
#' United States American Journal Potato Research 75, pag 211-217.
#' @keywords datasets
#' @examples
#' 
#' library(agricolae)
#' data(haynes)
#' str(haynes)
#' 
NULL