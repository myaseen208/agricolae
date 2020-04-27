#' Data of Bacterial Wilt (AUDPC) and soil
#' 
#' Percentage of bacterial wilt and area under the curve of disease progression
#' (AUDPC) relative tomato plants transplanted in different soil types
#' artificially infested with R.solanacearum 133 days before.
#' 
#' Percentajes bacterial wilt. Day7 = evaluated to 7 days, Days11 = evaluated
#' to 11 days.  see data(soil) and data(ralstonia)
#' 
#' @name wilt
#' @docType data
#' @format A data frame with 13 observations on the following 15 variables.
#' \describe{ \item{list("place")}{a factor with levels \code{Chmar} \code{Chz}
#' \code{Cnt1} \code{Cnt2} \code{Cnt3} \code{Hco1} \code{Hco2} \code{Hco3}
#' \code{Hyo1} \code{Hyo2} \code{Namora} \code{SR1} \code{SR2}}
#' \item{list("Day7")}{a numeric vector} \item{list("Day11")}{a numeric vector}
#' \item{list("Day15")}{a numeric vector} \item{list("Day19")}{a numeric
#' vector} \item{list("Day23")}{a numeric vector} \item{list("Day27")}{a
#' numeric vector} \item{list("Day31")}{a numeric vector}
#' \item{list("Day35")}{a numeric vector} \item{list("Day39")}{a numeric
#' vector} \item{list("Day43")}{a numeric vector} \item{list("Day47")}{a
#' numeric vector} \item{list("Day51")}{a numeric vector}
#' \item{list("AUDPC")}{a numeric vector} \item{list("relative")}{a numeric
#' vector} }
#' @references International Potato Center. CIP - Lima Peru.
#' @source Experimental field, 2004. Data Kindly provided by Dr. Sylvie Priou,
#' Liliam Gutarra and Pedro Aley.
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