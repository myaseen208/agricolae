#' @title Data of corn
#' @name   corn
#' @docType data
#' @keywords datasets
#' @usage data(corn)
#' @description
#' Data from a completely randomized design where four different methods of
#' growing corn resulted in various yields per acre on various plots of ground
#' where the four methods were tried. Ordinarily, only one statistical analysis
#' is used, but here we will use the kuskal-wallis test so that a rough
#' comparison may be made with the mediasn test.
#' 
#' The observations are ranked from the smallest, 77, of rank 1 to the largest
#' 101, of rank N=34. Ties values receive the averarge rank.
#' 
#' @format 
#' A \code{data.frame} with 34 observations on the following 3 variables.
#' 
#'  @details
#'         \itemize{
#'         \item{\strong{method}} method
#'         \item{\strong{v}} observation
#'         \item{\strong{rx}} rx
#'         \item{\strong{E9}} infestans percentage to 9 days
#'         \item{\strong{AUDPC}} the area under the disease-progress curve
#'         \item{\strong{Relative}}     relative area
#'         }
#' 
#' @references 
#' Practical Nonparametrics Statistics. W.J. Conover. Third Edition, 1999.
#' 
#' @source
#' Book: Practical Nonparametric Statistics.
#' 
#' @keywords datasets
#' @examples
#' 
#' data(corn)
#' str(corn)
#' 
NULL