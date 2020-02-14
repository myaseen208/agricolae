\name{DC}
\alias{DC}
\docType{data}
\title{ Data for the analysis of carolina genetic design}
\description{
   Data for the analysis of carolina I, II and III genetic design
}
\usage{data(DC)}
\details{
DC is list, 3 data.frame: carolina1(72 obs, 6 var), carolina2(300 obs, 9 var) and  carolina3(64 obs, 5 var).
 
 Carolina1: Data for the analysis of Carolina I Genetic design. In this design F2 or any advanced generation maintained by random mating, 
 produced from cross between two pure-lines, is taken as base population.
 From the population an individual is randomly selected and used as a male.
 A set of 4 randomly selected plans are used as females and are mated to the
 above male. Thus a set of 4 full-sib families are produced. This is denoted
 as a male group. Similarly, a large number of male groups are produced. 
 No female is used for any second mating. four male groups (16 female groups)
 from a set.
 
 Carolina2: Data for the analysis of Carolina II Genetic design.
 Both paternal and maternal half-sibs are produced in this design.
 From an F2 population, n1 males and n2 females are randomly selected
 and each male is crossed to each of the females. Thus n1 x n2 progenies
 are produced whitch are analysed in a suitably laid experiment.
 
 Carolina3: Data for the analysis of Carolina III genetic design. The F2 
 population is produced by crossing two inbreds, say L1 and L2. 
 The material for estimation of genetic parameters is produced 
 by back crossing randomly selected F2 individuals (using as males)
 to each of the inbreds (used as females).
  
}
\source{
 Biometrical Methods in Quantitative Genetic Analysis, Singh, Chaudhary. 1979.
}
\references{
 Biometrical Methods in Quantitative Genetic Analysis, Singh, Chaudhary. 1979.
}
\examples{
data(DC)
names(DC)
str(DC$carolina1)
str(DC$carolina2)
str(DC$carolina3)
}
\keyword{datasets}
