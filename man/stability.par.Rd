\name{stability.par}
\alias{stability.par}
%- stability.par.
\title{ Stability analysis. SHUKLA'S STABILITY VARIANCE AND KANG'S }
\description{
  This procedure calculates the stability variations as well as the statistics 
  of selection for the yield and the stability.
  The averages of the genotype through the different environment repetitions 
  are required for the calculations. The mean square error must be calculated
  from the joint variance analysis.
}
\usage{
stability.par(data,rep,MSerror,alpha=0.1,main=NULL,cova = FALSE,name.cov=NULL,
file.cov=0,console=FALSE)
}

\arguments{
  \item{data}{ matrix of averages, by rows the genotypes and columns the environment }
  \item{rep}{ Number of repetitions }
  \item{MSerror}{ Mean Square Error }
  \item{alpha}{ Label significant }
  \item{main}{ Title }
  \item{cova}{ Covariable }
  \item{name.cov}{ Name covariable }
  \item{file.cov}{ Data covariable }
  \item{console}{logical, print output }
}

\value{
  \item{analysis}{ Analysis of variance }
  \item{statistics}{ Statistics of the model }
  \item{stability}{ summary stability analysis }
}
\details{
Stable 
(i) determines the contribution of each genotype to GE interaction by calculating var(i);
(ii) assigns ranks to genotypes from highest to lowest yield receiving the rank of 1;
(iii) calculates protected LSD for mean yield comparisons;
(iv) adjusts yield rank according to LSD (the adjusted rank labeled Y);
(v) determines significance of var(i) usign an aproximate F-test;
(vi) assigns stability rating (S) as follows: -8, -4 and -2 for var(i) significant at the
0.01, 0.05 and 0.10 probability levels, and 0 for nonsignificant var(i) 
( the higher the var(i), the less stable the genotype);
(vii) sums adjusted yield rank, Y, and stability rating, S, for each genotype to determine YS(i) statistic; and 
(viii) calculates mean YS(i) and identifies genotypes (selection) with YS(i) > mean YS(i).
}
\references{ Kang, M. S. 1993. Simultaneous selection for yield and
 stability: Consequences for growers. Agron. J. 85:754-757. 
 Manjit S. Kang and Robert Mangari. 1995. Stable: A basic program for 
 calculating stability and yield-stability statistics. Agron. J. 87:276-277}
\author{ Felipe de Mendiburu }

\seealso{\code{\link{stability.nonpar} } }

\examples{
library(agricolae)
# example 1
# Experimental data,
# replication rep= 4
# Mean square error, MSerror = 1.8
# 12 environment
# 17 genotype  = 1,2,3,.., 17
# yield averages of 13 genotypes in localities
f <- system.file("external/dataStb.csv", package="agricolae")
dataStb<-read.csv(f)
stability.par(dataStb, rep=4, MSerror=1.8, alpha=0.1, main="Genotype",console=TRUE)

#example 2 covariable. precipitation
precipitation<- c(1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,2100)
stability.par(dataStb, rep=4, MSerror=1.8, alpha=0.1, main="Genotype",
 cova=TRUE, name.cov="Precipitation", file.cov=precipitation,console=TRUE)
}
\keyword{ models }% at least one, from doc/KEYWORDS

