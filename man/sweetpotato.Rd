\name{sweetpotato}
\alias{sweetpotato}
\docType{data}
\title{ Data of sweetpotato yield }
\description{
 The data correspond to an experiment with costanero sweetpotato made at the locality 
 of the Tacna department, southern Peru. The effect of two viruses (Spfmv and Spcsv) 
 was studied. The treatments were the following: CC (Spcsv) = Sweetpotato chlorotic 
 dwarf, FF (Spfmv) = Feathery mottle, FC (Spfmv y Spcsv) = Viral complex and OO 
 (witness) healthy plants.
 In each plot, 50 sweetpotato plants were sown and 12 plots were employed. 
 Each treatment was made with 3 repetitions and at the end of the experiment the total 
 weight in kilograms was evaluated. The virus transmission was made in the cuttings 
 and these were sown in the field.
}
\usage{data(sweetpotato)}
\format{
  A data frame with 12 observations on the following 2 variables.
  \describe{
    \item{\code{virus}}{a factor with levels \code{cc} \code{fc} \code{ff} \code{oo}}
    \item{\code{yield}}{a numeric vector}
  }
}

\source{
  Experimental field.
}
\references{
  International Potato Center. CIP - Lima Peru
}
\examples{
library(agricolae)
data(sweetpotato)
str(sweetpotato)
}
\keyword{datasets}
