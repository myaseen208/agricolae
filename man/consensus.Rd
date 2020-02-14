\name{consensus}
\alias{consensus}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ consensus of clusters }
\description{
  The criterion of the consensus is to produce many trees by means of
  boostrap and to such calculate the relative frequency with members of the clusters.
}
\usage{
consensus(data,distance=c("binary","euclidean","maximum","manhattan",
"canberra", "minkowski", "gower","chisq"),method=c("complete","ward","single","average",
"mcquitty","median", "centroid"),nboot=500,duplicate=TRUE,cex.text=1, 
col.text="red", ...)
}
\arguments{
  \item{data}{ data frame }
  \item{distance}{ method distance, see dist() }
  \item{method}{ method cluster, see hclust() }
  \item{nboot}{ The number of bootstrap samples desired. }
  \item{duplicate}{ control is TRUE other case is FALSE }
  \item{cex.text}{ size text on percentage consensus }
  \item{col.text}{ color text on percentage consensus }
  \item{\dots}{ parameters of the plot dendrogram }
}
\details{
  distance: "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski", "gower", "chisq".
  Method: "ward", "single", "complete", "average", "mcquitty", "median", "centroid".
  see functions: dist(), hclust() and daisy() of cluster.
}
\value{
  \item{table.dend }{The groups and consensus percentage}
  \item{dendrogram}{The class object is hclust, dendrogram plot}
  \item{duplicate}{Homonymous elements}  
}
\references{ An Introduction to the Boostrap. Bradley Efron
and Robert J. Tibshirani. 1993. Chapman and Hall/CRC }
\author{ F. de Mendiburu }

\seealso{ \code{\link{hclust}}, \code{\link{hgroups}}, \code{\link{hcut}} }
\examples{
library(agricolae)
data(pamCIP)
# only code
rownames(pamCIP)<-substr(rownames(pamCIP),1,6)
output<-consensus( pamCIP,distance="binary", method="complete",nboot=5)
# Order consensus
Groups<-output$table.dend[,c(6,5)]
Groups<-Groups[order(Groups[,2],decreasing=TRUE),]
print(Groups)
## Identification of the codes with the numbers.
cbind(output$dendrogram$labels)
## To reproduce dendrogram
dend<-output$dendrogram
data<-output$table.dend
plot(dend)
text(data[,3],data[,4],data[,5])
# Other examples
# classical dendrogram
dend<-as.dendrogram(output$dendrogram)
plot(dend,type="r",edgePar = list(lty=1:2, col=2:1))
text(data[,3],data[,4],data[,5],col="blue",cex=1)
plot(dend,type="t",edgePar = list(lty=1:2, col=2:1))
text(data[,3],data[,4],data[,5],col="blue",cex=1)
## Without the control of duplicates
output<-consensus( pamCIP,duplicate=FALSE,nboot=5)
## using distance gower, require cluster package.
# output<-consensus( pamCIP,distance="gower", method="complete",nboot=5)
}

\keyword{ cluster }
