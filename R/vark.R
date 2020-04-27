#' Variance K, ties, Kendall
#' 
#' The Kendall method in order to find the K variance.
#' 
#' Script in C to R.
#' 
#' @param x Vector
#' @param y vector
#' @return variance of K for Kendall's tau
#' @author Felipe de Mendiburu
#' @seealso cor.matrix, cor.vector, cor.mv
#' @references Numerical Recipes in C. Second Edition.
#' @keywords nonparametric
#' @export
#' @examples
#' 
#' library(agricolae)
#' x <-c(1,1,1,4,2,2,3,1,3,2,1,1,2,3,2,1,1,2,1,2)
#' y <-c(1,1,2,3,4,4,2,1,2,3,1,1,3,4,2,1,1,3,1,2)
#' vark(x,y)
#' 
vark <-
function(x, y)
    {
        ties.x <- rle(sort(x))$lengths
        ties.y <- rle(sort(y))$lengths
        n <- length(x)
        t1 <- n * (n - 1) * (2 * n + 5)
        t2 <- sum(ties.x * (ties.x - 1) * (2 * ties.x + 5))
        t3 <- sum(ties.y * (ties.y - 1) * (2 * ties.y + 5))
        v1 <- (t1 - t2 - t3)/18
        t1 <- sum(ties.x * (ties.x - 1) * (ties.x - 2))
        t2 <- sum(ties.y * (ties.y - 1) * (ties.y - 2))
        v2 <- (t1 * t2)/(9 * n * (n - 1) * (n - 2))
        t1 <- sum(ties.x * (ties.x - 1)) * sum(ties.y * (ties.y - 1))
        v3 <- t1/(2 * n * (n - 1))
        v1 + v2 + v3
    }
