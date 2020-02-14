\name{index.AMMI}
\alias{index.AMMI}
%- AMMI.
\title{ AMMI index and yield stability}
\description{
calculate AMMI stability value (ASV) and Yield stability index (YSI).
}
\usage{
index.AMMI(model)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{ object AMMI }
}
\details{
AMMI stability value (ASV) was calculated using the following formula,
as suggested by Purchase (1997)

 ASV = sqrt((SSpc1/SSpc2 * PC1i)^2+(PC2i)^2)

 YSI = RASV + RY
 
 RASV = rank(ASV) and RY = rank(Y across by environment)
}
\value{
  \item{ASV}{AMMI stability value}
  \item{YSI}{Yield stability index}
  \item{rASV}{Rank of AMMI stability value}
  \item{rYSI}{Rank of yield stability index}
  \item{means}{average genotype by environment}
}
\references{ 
The use of an AMMI model and its parameters to analyse yield stability
in multienvironment trials. N. SABAGHNIA, S.H. SABAGHPOUR
AND H. DEHGHANI. Journal of Agricultural Science (2008), 146, 571-581.
f 2008 Cambridge University Press 571 doi:10.1017/S0021859608007831
Printed in the United Kingdom

Parametric analysis to describe genotype  x environment interaction and yield
stability in winter wheat. PURCHASE, J. L. (1997).
Ph.D. Thesis, Department of Agronomy, Faculty of Agriculture of the
University of the Free State, Bloemfontein, South Africa.
 }
\author{ F. de Mendiburu }

\seealso{\code{\link{AMMI}},\code{\link{plot.AMMI}} }
\examples{
library(agricolae)
# Index AMMI
data(plrv)
model<- with(plrv,AMMI(Locality, Genotype, Rep, Yield, console=FALSE))
Idx<-index.AMMI(model)
names(Idx)
# Crops with improved stability according AMMI.
print(Idx[order(Idx[,3]),])
# Crops with better response and improved stability according AMMI.
print(Idx[order(Idx[,4]),])
}
\keyword{ models }
