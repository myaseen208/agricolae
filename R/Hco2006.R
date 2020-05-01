#' @title Data amendment Huanuco 2006
#' @name   Hco2006
#' @docType data
#' @keywords datasets
#' @usage data(Hco2006)
#' @description
#' Incidents and performance of healthy tubers and rotten potato field infested
#' with naturally Ralstonia solanacearum Race 3/Bv 2A, after application of
#' inorganic amendments and a rotation crop in Huanuco Peru, 2006.
#' 
#' Application of inorganic amendment and crop rotation to control bacterial
#' wilt of the potato (MBP).
#' 
#' @format An object of class \code{list} with two elements: wilt and yield
#'
#'  @details
#'         \itemize{
#'         \item{\strong{amendment}} amendment
#'         \item{\strong{crop}} crop
#'         \item{\strong{block}} block
#'         \item{\strong{plant}} number platn
#'         \item{\strong{wilt_percent}} wilt percentage at 60 days
#'         \item{\strong{health}}     kg/8m2, 20 plants
#'         \item{\strong{rot}}     kg/8m2, 20 plants
#'         }
#' 
#' @references 
#' International Potato Center. CIP - Lima Peru.
#' 
#' @source 
#' Experimental field, 2006. Data Kindly provided by Pedro Aley.
#' 
#' @keywords datasets
#' @examples
#' 
#' library(agricolae)
#' data(Hco2006)
#' str(Hco2006)
#' wilt<-Hco2006$wilt
#' yield<-Hco2006$yield
#' means <- tapply.stat(wilt[,5],wilt[,1:3],function(x) mean(x,na.rm=TRUE))
#' names(means)[4]<-"wilt_percent"
#' model <- aov(wilt_percent ~ block + crop, means)
#' anova(model)
#' cv.model(model)
#' yield<-yield[order(paste(yield[,1],yield[,2],yield[,3])),]
#' correlation(means[,4],yield[,4],method="spearman")
#' 
NULL