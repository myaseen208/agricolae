\name{similarity}
\alias{similarity}
%- similarity.
\title{ Matrix of similarity in binary data }
\description{
 It finds the similarity matrix of binary tables (1 and 0).
}
\usage{
similarity(A)
}

\arguments{
  \item{A}{ Matrix, data binary }
}
\value{
Distance matrix. Class = dist.
}
\author{ Felipe de Mendiburu }

\seealso{\code{\link{cv.similarity}}, \code{\link{resampling.cv} } }

\examples{
#example table of molecular markers
library(agricolae)
data(markers)
distance<-similarity(markers)
#startgraph
tree<-hclust(distance,method="mcquitty")
plot(tree,col="blue")
#endgraph
}
\keyword{ models }% at least one, from doc/KEYWORDS

