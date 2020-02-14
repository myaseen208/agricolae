\name{index.smith}
\alias{index.smith}
%- index.smith.
\title{ Uniformity soil. Smith's Index of Soil Heterogeneity }
\description{
  Smith's index of soil heterogeneity is used primarily to derive optimum
  plot size. The index gives a single value as a quantitative measure of 
  soil heterogeneity in an area.
  Graph CV for plot size and shape.
}
\usage{
index.smith(data, PLOT=TRUE,...)
}

\arguments{
  \item{data}{ dataframe or matrix }
  \item{PLOT}{ graphics, TRUE or FALSE }
 \item{\dots}{ Parameters of the plot() }
}
\details{
 
 Vx=V(x)/x b
 

 V(x) is the between-plot variance,
 Vx is the variance per unit area for plot size of x basic unit, 
 and b is the Smith' index of soil heterogeneity.
}
\value{
  \item{model}{function pattern of uniformity}
  \item{uniformity}{Table of the soil uniformity}
}
\references{ Statistical Procedures for Agriculture Research. Second Edition.
Kwanchai A. Gomez and Arturo A. Gomez. 1976. USA
 }
\author{ Felipe de Mendiburu }

\examples{
library(agricolae)
data(rice)
#startgraph
table<-index.smith(rice,
 main="Relationship between CV per unit area and plot size",col="red")
#endgraph
uniformity <- data.frame(table$uniformity)
uniformity
# regression variance per unit area an plot size.
model <- lm(Vx ~ I(log(Size)),uniformity)
coeff <- coef(model)
x<-1:max(uniformity$Size)
Vx<- coeff[1]+coeff[2]*log(x)
#startgraph
plot(x,Vx, type="l", col="blue",
 main="Relationship between variance per unit area and plot size")
points(uniformity$Size,uniformity$Vx)
#endgraph
}
\keyword{ design }

