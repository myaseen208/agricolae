#' @name    AMMI.contour
#' @aliases AMMI.contour
#' @title AMMI contour
#' @description Draws a polygon or a circumference around the center of the Biplot with a proportional radio at the longest distance of the genotype.
#' 
#' @param \strong{model} Object
#' @param \strong{distance} Circumference radius >0 and <=1 
#' @param \strong{shape} Numerical, relating to the shape of the polygon outline.
#' @param \strong{\dots} Parameters corresponding to the R lines function
#' 
#' @details First, it is necessary to execute the AMMI function.
#' It is only valid for the BIPLOT function but not for the TRIPLOT one. 
#' 
#' @return \strong{Genotypes} within and outside the area.
#' @return \strong{distance} Distance from genotype to origin (0,0)
#' 
#' @author
#' \enumerate{
#'          \item Felipe de Mendiburu (\email{fmendiburu@@lamolina.edu.pe})
#'          }
#'           
#' @note Complement graphics AMMI
#' 
#' @seealso \code{\link{AMMI}}
#' 
#' @keywords plot
#' 
#' @examples                   
#' library(agricolae)
#' # see AMMI.
#' data(sinRepAmmi)
#' Environment <- sinRepAmmi$ENV
#' Genotype <- sinRepAmmi$GEN
#' Yield <- sinRepAmmi$YLD
#' REP <- 3
#' MSerror <- 93.24224
#' model <- AMMI(Environment, Genotype, REP, Yield, MSerror)
#' plot(model)
#' AMMI.contour(model,distance=0.7,shape=8,col="red",lwd=2,lty=5)

AMMI.contour <- function(model, distance, shape, ...){
  UseMethod("AMMI.contour")
}
#' @export
#' @rdname AMMI.contour

AMMI.contour.default <- function(model, distance, shape, ...) {
  G <- subset(model$biplot, model$biplot$type == "GEN")
  x <- G$PC1
  y <- G$PC2
  d <- sqrt(x^2+y^2)
  r <- distance*max(d)
  x <- seq(-r, r, length = shape)
  A <- cbind(x, y = sqrt(r^2-x^2))
  B <- cbind(x, y = -sqrt(r^2-x^2))
  lines(A, type = "l" ,...)
  lines(B, type = "l" ,...)
  Gin <- d <= r
  Gout <- d > r
  GEN.in <- row.names(G)[Gin]
  GEN.out <- row.names(G)[Gout]
  cat("\nLimit, radio:", r)
  cat("\nGenotype  in:", length(GEN.in))
  cat("\nGenotype out:", length(GEN.out), "\n\n")
  distance <- data.frame(row.names = row.names(G), distance = d)
  
  return(
    list(
      "GENOTYPE IN"  = GEN.in
    , "GENOTYPE OUT" = GEN.out
    , Distance       = distance
    )
  )
  }
