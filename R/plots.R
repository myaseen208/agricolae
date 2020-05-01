#' @title Data for an analysis in split-plot
#' @name   plots
#' @docType data
#' @keywords datasets
#' @usage data(plots)
#' @description
#' Experimental data in blocks, factor A in plots and factor B in sub-plots.
#' 
#' 
#' @format 
#' A \code{data.frame} with 18 observations on the following 5 variables.
#' 
#'  @details
#'         \itemize{
#'         \item{\strong{block}} block
#'         \item{\strong{plot}} a factor with levels p1, p2, p3, p4, p5, and p6
#'         \item{\strong{A}} a factor with levels a1, and a2
#'         \item{\strong{B}} a factor with levels b1, b2, and b3
#'         \item{\strong{yield}} yield
#'         }
#' 
#' 
#' @source 
#' International Potato Center. CIP
#' 
#' @keywords datasets
#' @examples
#' 
#' library(agricolae)
#' data(plots)
#' str(plots)
#' plots[,1] <-as.factor(plots[,1])
#' # split-plot analysis
#' model <- aov(yield ~ block + A + Error(plot)+ B + A:B, data=plots)
#' summary(model)
#' b<-nlevels(plots$B)
#' a<-nlevels(plots$A)
#' r<-nlevels(plots$block)
#' dfa <- df.residual(model$plot)
#' Ea <-deviance(model$plot)/dfa
#' dfb <- df.residual(model$Within)
#' Eb <-deviance(model$Within)/dfb
#' Eab <- (Ea +(b-1)*Eb)/(b*r)
#' # Satterthwaite
#' dfab<-(Ea +(b-1)*Eb)^2/(Ea^2/dfa +((b-1)*Eb)^2/dfb)
#' # Comparison A, A(b1), A(b2), A(b3)
#' comparison1 <-with(plots,LSD.test(yield,A,dfa,Ea))
#' comparison2 <-with(plots,LSD.test(yield[B=="b1"],A[B=="b1"],dfab,Eab))
#' comparison3 <-with(plots,LSD.test(yield[B=="b2"],A[B=="b2"],dfab,Eab))
#' comparison4 <-with(plots,LSD.test(yield[B=="b3"],A[B=="b3"],dfab,Eab))
#' # Comparison B, B(a1), B(a2)
#' comparison5 <-with(plots,LSD.test(yield,B,dfb,Eb))
#' comparison6 <-with(plots,LSD.test(yield[A=="a1"],B[A=="a1"],dfb,Eb))
#' comparison7 <-with(plots,LSD.test(yield[A=="a2"],B[A=="a2"],dfb,Eb))
#' 
NULL