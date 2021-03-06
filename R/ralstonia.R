#' @title Data of assessment of the population in the soil R.solanacearum
#' @name   ralstonia
#' @docType data
#' @keywords datasets
#' @usage data(ralstonia)
#' @description
#' The assessment of the population of R.solanacearum on the floor took place
#' after 48 hours of infestation, during days 15, 29, 43, 58, and 133 days
#' after the infestation soil. More information on soil data(soil).
#' 
#' Logarithm average counts of colonies on plates containing half of M-SMSA 3
#' repetitions (3 plates by repetition) incubated at 30 degrees centigrade for
#' 48 hours. log(1+UFC/g soil)
#' 
#' @format 
#' A \code{data.frame} with 13 observations on the following 8 variables.
#' 
#' #'  @details
#'         \itemize{
#'         \item{\strong{place}}  a factor with levels 
#'                        Chmar, Chz, Cnt1, Cnt2, Cnt3, 
#'                        Hco1, Hco2, Hco3, Hyo1, Hyo2, Namora, SR1, and SR2
#'         \item{\strong{Day2}} Day2
#'         \item{\strong{Day15}} Day15
#'         \item{\strong{Day29}} Day29
#'         \item{\strong{Day43}} Day43
#'         \item{\strong{Day58}} Day58
#'         \item{\strong{Day73}} Day73
#'         \item{\strong{Day133}} Day133
#'         }
#'         
#' @references 
#' International Potato Center. CIP - Lima Peru.
#' 
#' @source 
#' Experimental field, 2004. Data Kindly provided by Dr. Sylvie Priou,
#' Liliam Gutarra and Pedro Aley.
#' 
#' @keywords datasets
#' @examples
#' 
#' library(agricolae)
#' data(ralstonia)
#' str(ralstonia)
#' 
NULL