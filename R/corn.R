#' Data of corn
#' 
#' Data from a completely randomized design where four different methods of
#' growing corn resulted in various yields per acre on various plots of ground
#' where the four methods were tried. Ordinarily, only one statistical analysis
#' is used, but here we will use the kuskal-wallis test so that a rough
#' comparison may be made with the mediasn test.
#' 
#' The observations are ranked from the smallest, 77, of rank 1 to the largest
#' 101, of rank N=34. Ties values receive the averarge rank.
#' 
#' @name corn
#' @docType data
#' @format A data frame with 34 observations on the following 3 variables.
#' \describe{ \item{list("method")}{a numeric vector}
#' \item{list("observation")}{a numeric vector} \item{list("rx")}{a numeric
#' vector} }
#' @references Practical Nonparametrics Statistics. W.J. Conover. Third
#' Edition, 1999.
#' @source Book: Practical Nonparametric Statistics.
#' @keywords datasets
#' @examples
#' 
#' data(corn)
#' str(corn)
#' 
NULL