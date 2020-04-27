#' Normal curve on the histogram
#' 
#' A normal distribution graph elaborated from the histogram previously
#' constructed. The average and variance are obtained from the data grouped in
#' the histogram.
#' 
#' 
#' @param histogram object constructed by the function hist
#' @param frequency 1=counts, 2=relative, 3=density
#' @param \dots Other parameters of the function hist
#' @author Felipe de Mendiburu
#' @seealso \code{\link{polygon.freq}}, \code{\link{table.freq}},
#' \code{\link{stat.freq}}, \code{\link{intervals.freq}},
#' \code{\link{sturges.freq}}, \code{\link{join.freq}},
#' \code{\link{ogive.freq}}, \code{\link{graph.freq} }
#' @keywords aplot
#' @export
#' @examples
#' 
#' library(agricolae)
#' data(growth)
#' #startgraph
#' h1<-with(growth,hist(height,col="green",xlim=c(6,14)))
#' normal.freq(h1,col="blue")
#' #endgraph
#' #startgraph
#' h2<-with(growth,graph.freq(height,col="yellow",xlim=c(6,14),frequency=2))
#' normal.freq(h2,frequency=2)
#' #endgraph
#' 
normal.freq <-
function (histogram,frequency=1, ...)
{
xx <- histogram$mids
if(frequency==1)yy<-histogram$counts
if(frequency==2)yy<-histogram$counts/sum(histogram$counts)
if(frequency==3)yy<-histogram$density   
    media <- sum(yy * xx)/sum(yy)
    variancia <- sum(yy * (xx - media)^2)/sum(yy)
    zz <- histogram$breaks
    x1 <- xx[1] - 4 * (zz[2] - zz[1])
    z <- length(zz)
    x2 <- xx[z - 1] + 4 * (zz[z] - zz[z - 1])
    x <- seq(x1, x2, length = 200)
    y <- rep(0, 200)
    area <- 0
    for (k in 1:(z - 1)) area = area + yy[k] * (zz[k + 1] - zz[k])
    for (i in 1:200) {
        y[i] <- area * exp(-((x[i] - media)^2)/(2 * variancia))/sqrt(2 * 
            pi * variancia)
    }
    lines(x, y, ...)
    abline(h = 0)
}
