\name{huasahuasi}
\alias{huasahuasi}
\docType{data}
\title{ Data: Rainfall thresholds as support for timing fungicide applications in the control of potato late blight in Peru }
\description{
  Timing fungicide sprays based on accumulated rainfall thresholds can be a successful 
  component of integrated management packages that include cultivars with moderate 
  or high levels of resistance to late blight. The simplicity of measuring accumulated rainfall means that the technology 
  can potentially be used by resource-poor farmers in developing countries. 
}
\usage{data(huasahuasi)}
\format{
  The format is:
List of 2 ( AUDPC, YIELD )
\describe{
    \item{\code{block}}{a factor with levels \code{I} \code{II} \code{III}}
    \item{\code{trt}}{a factor with levels \code{40mm} \code{7-days} \code{Non-application}}
    \item{\code{clon}}{a factor with levels \code{C386209.10} \code{C387164.4} \code{Cruza148} \code{Musuq} \code{Yungay}}
    \item{\code{y1da}}{a numeric vector, Kgr./plot}
    \item{\code{y2da}}{a numeric vector, Kgr./plot}
    \item{\code{y3ra}}{a numeric vector, Kgr./plot}
    \item{\code{d44}}{a numeric vector, 44 days}
    \item{\code{d51}}{a numeric vector, 51 days}    
    \item{\code{d100}}{a numeric vector, 100 days}    
    }
}
\details{
  The experimental unit was formed by 4 furrows of 1.8 m of length, with distance 
  between furrows from 0.90 m and between plants of 0.30 m. In each furrow was 
  installed 5 plants. The experiment had 3 repetitions. 
  From the beginning of the experiment were fulfilled the following treatments
  Thresholds 40 mm: Apply the fungicide when 40 precipitation mm accumulates. 
  The minimum interval between applications will be of 7 days. 
  Schedule 7 days: The applications should be carried out every 7 days calendar. 
  Without application: No fungicide application will be made. 
  The evaluation of the severity of the late blight in each treatment started to 
  emergency 80 percentage and then evaluations were made every 7 days until being observed 
  a physiological maturation of the crop. 
}
\source{
  Experimental field, 2003. Data Kindly provided by Wilmer Perez.
}
\references{
  International Potato Center. CIP - Lima Peru.
}
\examples{
library(agricolae)
data(huasahuasi)
names(huasahuasi)
str(huasahuasi$AUDPC)
str(huasahuasi$YIELD)

}
\keyword{datasets}
