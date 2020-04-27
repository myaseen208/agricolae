#' Omitting the rows or columns with missing observations of a matrix (NA)
#' 
#' In many situations it is required to omit the rows or columns less or
#' greater with NA of the matrix.
#' 
#' 
#' @param x matrix with NA
#' @param alternative "less" or "greater"
#' @return \item{x }{matrix}
#' @author Felipe de Mendiburu
#' @keywords manip
#' @export
#' @examples
#' 
#' library(agricolae)
#' x<-c(2,5,3,7,5,NA,8,0,4,3,NA,NA)
#' dim(x)<-c(4,3)
#' x
#' #     [,1] [,2] [,3]
#' #[1,]    2    5    4
#' #[2,]    5   NA    3
#' #[3,]    3    8   NA
#' #[4,]    7    0   NA
#' 
#' delete.na(x,"less")
#' #     [,1]
#' #[1,]    2
#' #[2,]    5
#' #[3,]    3
#' #[4,]    7
#' 
#' delete.na(x,"greater")
#' #     [,1] [,2] [,3]
#' #[1,]    2    5    4
#' 
#' 
delete.na <-
function (x, alternative = c("less", "greater") )
{
if (alternative == "less")
{

    if (nrow(x) < ncol(x))
        a <- na.omit(x)
    else {
        a <- t(x)
        b <- na.omit(a)
        a <- t(b)
    }
    b <- cbind(a)
    return(b)
}
if (alternative == "greater")
{

    if (nrow(x) > ncol(x))
        a <- na.omit(x)
    else {
        a <- t(x)
        b <- na.omit(a)
        a <- t(b)
    }
    b <- cbind(a)
    return(b)
}
}
