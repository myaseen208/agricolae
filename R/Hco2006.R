#' Data amendment Huanuco 2006
#' 
#' Incidents and performance of healthy tubers and rotten potato field infested
#' with naturally Ralstonia solanacearum Race 3/Bv 2A, after application of
#' inorganic amendments and a rotation crop in Huanuco Peru, 2006.
#' 
#' Application of inorganic amendment and crop rotation to control bacterial
#' wilt of the potato (MBP).
#' 
#' @name Hco2006
#' @docType data
#' @format The format is: List of 2 \describe{ \item{list("amendment")}{a
#' factor} \item{list("crop")}{a factor} \item{list("block")}{a numeric vector,
#' replications} \item{list("plant")}{a numeric vector, number plant}
#' \item{list("wilt_percent")}{a numeric vector, wilt percentage at 60 days}
#' \item{list("health")}{a numeric vector, kg/8m2, 20 plants}
#' \item{list("rot")}{a numeric vector, kg/8m2, 20 plants} }
#' @references International Potato Center. CIP - Lima Peru.
#' @source Experimental field, 2006. Data Kindly provided by Pedro Aley.
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