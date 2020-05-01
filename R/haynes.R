#' @title Data of AUDPC for nonparametrical stability analysis
#' @name   haynes
#' @docType data
#' @keywords datasets
#' @usage data(haynes)
#' @description
#' Published data. Haynes. Mean area under the disease progress curve (AUDPC)
#' for each of 16 potato clones evaluated at eight sites across the United
#' States in 1996
#' 
#' 
#' @format 
#' A \code{data.frame} with 16 observations on the following 9 variables.
#'
#'  @details
#'         \itemize{
#'         \item{\strong{clone}} a factor with levels A84118-3,
#'                          AO80432-1, AO84275-3, AWN86514-2, B0692-4,
#'                          B0718-3, B0749-2F, B0767-2, Bertita, Bzura,
#'                          C0083008-1, Elba, Greta, Krantz, Libertas,
#'                          Stobrawa
#'         \item{\strong{FL}} FL
#'         \item{\strong{ME}} ME 
#'         \item{\strong{MI}} MI
#'         \item{\strong{MN}} MN
#'         \item{\strong{ND}} ND
#'         \item{\strong{NY}} NY
#'         \item{\strong{PA}} PA
#'         \item{\strong{WI}} WI
#'         }
#' 
#' @references 
#' Haynes K G, Lambert D H, Christ B J, Weingartner D P, Douches D
#' S, Backlund J E, Fry W and Stevenson W. 1998. Phenotypic stability of
#' resistance to late blight in potato clones evaluated at eight sites in the
#' United States American Journal Potato Research 75, pag 211-217.
#' 
#' @keywords datasets
#' @examples
#' 
#' library(agricolae)
#' data(haynes)
#' str(haynes)
#' 
NULL