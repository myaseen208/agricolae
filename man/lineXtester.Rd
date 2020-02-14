\name{lineXtester}
\alias{lineXtester}
%- lineXtester.
\title{ Line x Tester Analysis }
\description{
 It makes the Line x Tester Genetic Analysis. It also estimates the general and 
 specific combinatory ability effects and the line and tester genetic contribution.
 }

\usage{
lineXtester(replications, lines, testers, y)
}
\arguments{
  \item{replications}{ Replications}
  \item{lines}{ Lines }
  \item{testers}{ Testers }
  \item{y}{ Variable, response }
}
\details{
ANOVA with parents and crosses \cr
ANOVA for line X tester analysis \cr
ANOVA for line X tester analysis including parents \cr
GCA Effects: Lines Effects, Testers Effects and SCA Effects. \cr
Standard Errors for Combining Ability Effects. \cr
Genetic Components. \cr  
...\cr
Proportional contribution of lines, testers and their interactions to total variance 
}
\value{
return anova(formula = Y ~ Replications + Treatments).\cr
where the Treatments contains parents, crosses and crosses vs Parents.\cr
The crosses contains Lines, Testers and its interaction .
}
\references{ 
Biometrical Methods in Quantitative Genetic Analysis, Singh, Chaudhary. 1979.
Hierarchial and factorial mating designs for quantitative genetic analysis in tetrasomic potato. R. Ortis; A.Golmirzaie. Theor Appl Genet (2002) 104:675-679 }
\author{ Felipe de Mendiburu }
\seealso{\code{\link{AMMI} } }
\examples{
# see structure line by testers
library(agricolae)
# example 1
data(heterosis)
site1<-subset(heterosis,heterosis[,1]==1)
output1<-with(site1,lineXtester(Replication, Female, Male, v2))
# example 2
data(LxT)
str(LxT)
output2<-with(LxT,lineXtester(replication, line, tester, yield))
}
\keyword{ models }% at least one, from doc/KEYWORDS

