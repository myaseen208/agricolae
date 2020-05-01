#' @title Data: Rainfall thresholds as support for timing fungicide applications in
#' the control of potato late blight in Peru
#' @name   huasahuasi
#' @docType data
#' @keywords datasets
#' @usage data(huasahuasi)
#' @description
#' Timing fungicide sprays based on accumulated rainfall thresholds can be a
#' successful component of integrated management packages that include
#' cultivars with moderate or high levels of resistance to late blight. The
#' simplicity of measuring accumulated rainfall means that the technology can
#' potentially be used by resource-poor farmers in developing countries.
#' 
#' The experimental unit was formed by 4 furrows of 1.8 m of length, with
#' distance between furrows from 0.90 m and between plants of 0.30 m. In each
#' furrow was installed 5 plants. The experiment had 3 repetitions.  From the
#' beginning of the experiment were fulfilled the following treatments
#' Thresholds 40 mm: Apply the fungicide when 40 precipitation mm accumulates.
#' The minimum interval between applications will be of 7 days.  Schedule 7
#' days: The applications should be carried out every 7 days calendar.  Without
#' application: No fungicide application will be made.  The evaluation of the
#' severity of the late blight in each treatment started to emergency 80
#' percentage and then evaluations were made every 7 days until being observed
#' a physiological maturation of the crop.
#' 
#' @format 
#' An object of class \code{list} with two elements: AUDPC and YIELD
#' 
#'  @details
#'         \itemize{
#'         \item{\strong{block}}  a factor with levels I, II, and III
#'         \item{\strong{trt}}  a factor with levels 40mm, 7-days, and Non-application
#'         \item{\strong{clon}} a factor with levels C386209.10, C387164.4, Cruza148, Musuq, and Yungay
#'         \item{\strong{y1da}} y1da
#'         \item{\strong{y2da}} y2da
#'         \item{\strong{y3ra}} y3ra
#'         \item{\strong{d44}} 44 days
#'         \item{\strong{d51}} 51 days
#'         \item{\strong{d100}} 100 days
#'         }
#'         
#' @references 
#' International Potato Center. CIP - Lima Peru.
#' 
#' @source 
#' Experimental field, 2003. Data Kindly provided by Wilmer Perez.
#' 
#' @keywords datasets
#' @examples
#' 
#' library(agricolae)
#' data(huasahuasi)
#' names(huasahuasi)
#' str(huasahuasi$AUDPC)
#' str(huasahuasi$YIELD)
#' 
#' 
NULL