#' @title Data of Bacterial Wilt (AUDPC) and soil
#' @name   wilt
#' @docType data
#' @keywords datasets
#' @usage data(wilt)
#' @description
#' Percentage of bacterial wilt and area under the curve of disease progression
#' (AUDPC) relative tomato plants transplanted in different soil types
#' artificially infested with R.solanacearum 133 days before.
#' 
#' Percentajes bacterial wilt. Day7 = evaluated to 7 days, Days11 = evaluated
#' to 11 days.  see data(soil) and data(ralstonia)
#' 
#' @format 
#' A \code{data.frame} with 13 observations on the following 15 variables.
#' 
#' @details
#'         \itemize{
#'         \item{\strong{place}} a factor with levels 
#'               Chmar, Chz, Cnt1, Cnt2, Cnt3, 
#'               Hco1, Hco2, Hco3, Hyo1, Hyo2, Namora, SR1, and SR2
#'         \item{\strong{Day7}} Day7
#'         \item{\strong{Day11}} Day11
#'         \item{\strong{Day15}} Day15
#'         \item{\strong{Day19}} Day19
#'         \item{\strong{Day23}} Day23
#'         \item{\strong{Day27}} Day27
#'         \item{\strong{Day31}} Day31
#'         \item{\strong{Day35}} Day35
#'         \item{\strong{Day39}} Day39
#'         \item{\strong{Day43}} Day43
#'         \item{\strong{Day47}} Day47
#'         \item{\strong{Day51}} Day51
#'         \item{\strong{AUDPC}} the area under the disease-progress curve
#'         \item{\strong{relative}}     relative area
#'         }
#'         
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
#' data(wilt)
#' days<-c(7,11,15,19,23,27,31,35,39,43,47,51)
#' AUDPC<-audpc(wilt[,-1],days)
#' relative<-audpc(wilt[,-1],days,type="relative")
#' 
#' 
NULL