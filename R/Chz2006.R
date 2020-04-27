#' @title Data amendment Carhuaz 2006
#' @name   Chz2006
#' @docType data
#' @keywords datasets
#' @usage data(Chz2006)
#' @description Incidents and performance of healthy tubers and rotten potato field infested
#' with naturally Ralstonia solanacearum Race 3/Bv 2A, after application of
#' inorganic amendments and a rotation crop in Carhuaz Peru, 2006.
#' 
#' Application of inorganic amendment and crop rotation to control bacterial
#' wilt of the potato (MBP).
#' 
#' @format An object of class \code{list} with two elements: wilt and yield
#' 
#' @details
#'        \itemize{
#'        \item{\strong{amendment}} amendment
#'        \item{\strong{crop}} crop
#'        \item{\strong{block}} block
#'        \item{\strong{plant}} plant
#'        \item{\strong{wilt_percent}} a numeric vector, wilt percentage at 60 days
#'        \item{\strong{health}} a numeric vector, kg/8m2
#'        \item{\strong{rot}} a numeric vector, kg/8m2
#'        }
#'        
#' @references 
#' International Potato Center. CIP - Lima Peru.
#' 
#' @source 
#' Experimental field, 2006. Data Kindly provided by Pedro Aley.
#' 
#' 
#' @examples
#' 
#' library(agricolae)
#' data(Chz2006)
#' str(Chz2006)
#' wilt <- Chz2006$wilt
#' yield <- Chz2006$yield
#' means <- tapply.stat(wilt[,5], wilt[,1:3],function(x) mean(x,na.rm=TRUE))
#' names(means)[4]<-"wilt_percent"
#' model <- aov(wilt_percent ~ block + crop, means)
#' anova(model)
#' cv.model(model)
#' yield <- yield[order(paste(yield[,1],yield[,2],yield[,3])),]
#' correlation(means[,4],yield[,4],method="spearman")
#' 
#' 
NULL