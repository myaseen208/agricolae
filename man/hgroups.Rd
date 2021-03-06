% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hgroups.R
\name{hgroups}
\alias{hgroups}
\title{groups of hclust}
\usage{
hgroups(hmerge)
}
\arguments{
\item{hmerge}{The object is components of the hclust}
}
\value{
The merge clusters is printed.
}
\description{
Returns a vector with group memberships. This function is used by the
function consensus of clusters.
}
\examples{

library(agricolae)
data(pamCIP)
# only code
rownames(pamCIP)<-substr(rownames(pamCIP),1,6)
distance <- dist(pamCIP,method="binary")
clusters<- hclust( distance, method="complete")
# groups of clusters
hgroups(clusters$merge)

}
\seealso{
\code{\link{hclust}}, \code{\link{hcut}}, \code{\link{consensus}}
}
\author{
F. de Mendiburu
}
\keyword{cluster}
