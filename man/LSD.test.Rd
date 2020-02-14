\name{LSD.test}
\alias{LSD.test}
%- LSD.
\title{ Multiple comparisons, "Least significant difference" and Adjust P-values }
\description{
  Multiple comparisons of treatments by means of LSD and a grouping of treatments. 
  The level by alpha default is 0.05. Returns p-values adjusted using one of several methods
}
\usage{
LSD.test(y, trt, DFerror, MSerror, alpha = 0.05, p.adj=c("none","holm","hommel", 
"hochberg", "bonferroni", "BH", "BY", "fdr"), group=TRUE, main = NULL,console=FALSE)
}

\arguments{
  \item{y}{ model(aov or lm) or answer of the experimental unit  }
  \item{trt}{ Constant( only y=model) or vector treatment applied to each experimental unit  }
  \item{DFerror}{ Degrees of freedom of the experimental error }
  \item{MSerror}{ Means square error of the experimental  }
  \item{alpha}{ Level of risk for the test  }
  \item{p.adj}{ Method for adjusting p values (see p.adjust)  }  
  \item{group}{ TRUE or FALSE }
  \item{main}{ title of the study  }
  \item{console}{logical, print output }
}  
\details{
  For equal or different repetition.\cr
  For the adjustment methods, see the function p.adjusted.\cr
  p-adj ="none" is t-student.\cr
  
  It is necessary first makes a analysis of variance.\cr
  if model=y, then to apply the instruction:\cr 
LSD.test(model, "trt", alpha = 0.05, p.adj=c("none","holm","hommel", 
"hochberg", "bonferroni", "BH", "BY", "fdr"), group=TRUE, main = NULL,console=FALSE)\cr
  where the model class is aov or lm.
}
\value{
  \item{statistics}{Statistics of the model}
  \item{parameters}{Design parameters}
  \item{means}{Statistical summary of the study variable}
  \item{comparison}{Comparison between treatments}
  \item{groups}{Formation of treatment groups}
}
\references{
     Steel, R.; Torri,J; Dickey, D.(1997)
     Principles and Procedures of Statistics
     A Biometrical Approach. pp178.
     }
\author{ Felipe de Mendiburu }
\seealso{
  \code{\link{BIB.test}}, \code{\link{DAU.test}}, \code{\link{duncan.test}},
  \code{\link{durbin.test}}, \code{\link{friedman}}, \code{\link{HSD.test}},
  \code{\link{kruskal}}, \code{\link{Median.test}}, \code{\link{PBIB.test}}, 
  \code{\link{REGW.test}}, \code{\link{scheffe.test}}, \code{\link{SNK.test}},
  \code{\link{waerden.test}}, \code{\link{waller.test}}, \code{\link{plot.group}}
}
\examples{
library(agricolae)
data(sweetpotato)
model<-aov(yield~virus, data=sweetpotato)
out <- LSD.test(model,"virus", p.adj="bonferroni")
#stargraph
# Variation range: max and min
plot(out)
#endgraph
# Old version LSD.test()
df<-df.residual(model)
MSerror<-deviance(model)/df
out <- with(sweetpotato,LSD.test(yield,virus,df,MSerror))
#stargraph
# Variation interquartil range: Q75 and Q25
plot(out,variation="IQR")
#endgraph
out<-LSD.test(model,"virus",p.adj="hommel",console=TRUE)
plot(out,variation="SD") # variation standard deviation
}
\keyword{ htest }% at least one, from doc/KEYWORDS

