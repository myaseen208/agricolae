#' Class intervals for a histogram, the rule of Sturges
#' 
#' if k=0 then classes: k = 1 + log(n,2).  if k > 0, fixed nclass.
#' 
#' 
#' @param x vector
#' @param k constant
#' @return Statistics of sturges for a histogram.
#' @author Felipe de mendiburu
#' @seealso \code{\link{polygon.freq}}, \code{\link{table.freq}},
#' \code{\link{stat.freq}}, \code{\link{intervals.freq}},
#' \code{\link{graph.freq}}, \code{\link{join.freq}}, \code{\link{ogive.freq}},
#' \code{\link{normal.freq} }
#' @references Reza A. Hoshmand. 1988. Statistical Methods for Agricultural
#' Sciences, Timber Press, Incorporated, pag 18-21.
#' @keywords manip
#' @export
#' @examples
#' 
#' library(agricolae)
#' data(natives)
#' classes<-with(natives,sturges.freq(size))
#' # information of the classes
#' breaks <- classes$breaks
#' breaks
#' #startgraph
#' # Histogram with the established classes
#' h<-with(natives,graph.freq(size,breaks,frequency=1, col="yellow",axes=FALSE,
#'     xlim=c(0,0.12),main="",xlab="",ylab=""))
#' axis(1,breaks,las=2)
#' axis(2,seq(0,400,50),las=2)
#' title(main="Histogram of frequency\nSize of the tubercule of the Oca",
#' xlab="Size of the oca", ylab="Frequency")
#' #endgraph
#' 
sturges.freq <-
function (x,k=0) 
{
    n <- length(x)
    if (k==0) k <- round(1+log(n,2),0)
    p<- floor(log(abs(median(x,na.rm=TRUE)),10))
	x<-x/10^(p-1)
	maximo <- max(x,na.rm=TRUE)
	minimo <- min(x,na.rm=TRUE)
	min1<-floor(minimo)
	max1<-ceiling(maximo)
	amplitud <- max1 - min1
	tic <- round(amplitud/k,1)
	clases <- seq(min1, max1, tic)
    if (maximo > clases[length(clases)]) {
	clases <- c(clases, clases[length(clases)] + tic)
    }
	k <- length(clases)-1
	maximo<-maximo*10^(p-1);minimo<-minimo*10^(p-1);tic=tic*10^(p-1)
	clases<-clases*10^(p-1); amplitude=amplitud*10^(p-1)
    lista <- list(maximum = maximo, minimum = minimo, amplitude = amplitud, 
            classes = k, interval = tic, breaks = clases)
    return(lista)
}
