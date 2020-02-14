\name{paracsho}
\alias{paracsho}
\docType{data}
\title{ Data of Paracsho biodiversity }
\description{
A locality in Peru. A biodiversity.
}
\usage{data(paracsho)}
\format{
  A data frame with 110 observations on the following 6 variables.
  \describe{
    \item{\code{date}}{a factor with levels \code{15-12-05} \code{17-11-05} \code{18-10-05} \code{20-09-05} \code{22-06-05} \code{23-08-05} \code{28-07-05}}
    \item{\code{plot}}{a factor with levels \code{PARACSHO}}
    \item{\code{Treatment}}{a factor with levels \code{CON} \code{SIN}}
    \item{\code{Orden}}{a factor with levels \code{COLEOPTERA} \code{DIPTERA} \code{HEMIPTERA} \code{HYMENOPTERA} \code{LEPIDOPTERA} \code{NEUROPTERA} \code{NEUROPTERO} \code{NOCTUIDAE}}
    \item{\code{Family}}{a factor with levels \code{AGROMYZIDAE} \code{ANTHOCORIDAE} \code{ANTHOMYIIDAE} \code{ANTHOMYLIDAE} \code{BLEPHAROCERIDAE} \code{BRACONIDAE} \code{BROCONIDAE} \code{CALUPHORIDAE} \code{CECIDOMYIDAE} \code{CHENEUMONIDAE} \code{CHNEUMONIDAE} \code{CHRYOMELIDAE} \code{CICADELLIDAE} \code{CULICIDAE} \code{ERIOCPAMIDAE} \code{HEMEROBIIDAE} \code{ICHNEUMONIDAE} \code{LOUCHAPIDAE} \code{MIRIDAE} \code{MUSCIDAE} \code{MUSICADAE} \code{MUSLIDAE} \code{MYCETOPHILIDAE} \code{MYCETOPHILIIDAE} \code{NENPHALIDAE} \code{NOCLUIDAE} \code{NOCTERIDAE} \code{NOCTUIDAE} \code{PERALIDAE} \code{PIPUNCULIDAE} \code{PROCTOTRUPIDAE} \code{PSYLLIDAE} \code{PYRALIDAE} \code{SARCOPHAGIDAE} \code{SARCOPILAGIDAE} \code{SCATOPHAGIDAE} \code{SCATOPHOGIDAE} \code{SCIARIDAE} \code{SERSIDAE} \code{SYRPHIDAE} \code{TACHINIDAE} \code{TIPULIDAE}}
    \item{\code{Number.of.specimens}}{a numeric vector}
  }
}
\details{
  Country Peru, Deparment Junin, province Tarma, locality Huasahuasi.
  }
\source{
  Entomology dataset.
}
\references{
  International Potato Center.
}
\examples{
library(agricolae)
data(paracsho)
str(paracsho)
}
\keyword{datasets}
