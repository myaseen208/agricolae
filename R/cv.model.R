#' Coefficient of the experiment variation
#' 
#' It obtains the coefficient of variation of the experiment obtained by models
#' lm() or aov()
#' 
#' sqrt(MSerror)*100/mean(x)
#' 
#' @param x object of model lm() or AOV()
#' @return Returns the coefficient of variation of the experiment according to
#' the applied statistical model
#' @author Felipe de Mendiburu
#' @seealso \code{\link{LSD.test}}, \code{\link{HSD.test}},
#' \code{\link{waller.test}}
#' @keywords univar
#' @export
#' @examples
#' 
#' # see examples from LSD , Waller-Duncan or HSD and complete with it:
#' library(agricolae)
#' # not run
#' # cv<-cv.model(model)
#' 
#' 
cv.model <-
function(x) {
suma2 <- sum(x$residual^2)
gl <- x$df.residual
promedio<-mean(x$fitted.values)
return(sqrt(suma2/gl)*100/promedio)
}
