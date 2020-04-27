#' Join class for histogram
#' 
#' In many situations it is required to join classes because of the low
#' .frequency in the intervals. In this process, it is required to join the
#' intervals and ad the .frequencies of them.
#' 
#' 
#' @param histogram Class graph.freq
#' @param join vector
#' @return New histogram with union of classes.
#' @author Felipe de Mendiburu
#' @seealso \code{\link{polygon.freq}}, \code{\link{table.freq}},
#' \code{\link{stat.freq}}, \code{\link{intervals.freq}},
#' \code{\link{sturges.freq}}, \code{\link{graph.freq}},
#' \code{\link{ogive.freq}}, \code{\link{normal.freq} }
#' @keywords univar
#' @export
#' @examples
#' 
#' library(agricolae)
#' data(natives)
#' # histogram
#' h1<-graph.freq(natives$size,plot=FALSE)
#' round(table.freq(h1),4)
#' # Join classes  9, 10,11 and 12 with little frequency.
#' h2<-join.freq(h1,9:12)
#' # new table
#' plot(h2,col="bisque",xlab="Size")
#' round(summary(h2),4)
#' 
join.freq <-
function(histogram,join){
classes<-histogram$breaks
frec<-histogram$counts
frec[join[1]]<-sum(frec[join])
join<-join[-1]
classes<-classes[-join]
frec<-frec[-join]
h<-graph.freq(classes,counts=frec,plot=FALSE)
return(h)
}
