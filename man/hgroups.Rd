\name{hgroups}
\alias{hgroups}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ groups of hclust }
\description{
  Returns a vector with group memberships. This function is used 
  by the function consensus of clusters.
}
\usage{
hgroups(hmerge)
}
\arguments{
  \item{hmerge}{ The object is components of the hclust}
}
\value{
The merge clusters is printed.
}
\author{ F. de Mendiburu }

\seealso{\code{\link{hclust}}, \code{\link{hcut}}, \code{\link{consensus}} }

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

\keyword{ cluster }
