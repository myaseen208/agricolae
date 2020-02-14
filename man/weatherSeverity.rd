\name{weatherSeverity}
\alias{weatherSeverity}
\title{
Weather and Severity
}
\description{
Weather and Severity
}
\usage{
weatherSeverity(weather,severity,dates,EmergDate,EndEpidDate,NoReadingsH,
RHthreshold)
}
\arguments{
  \item{weather}{object, see example}
  \item{severity}{object, see example }
  \item{dates}{vector dates}
  \item{EmergDate}{date}
  \item{EndEpidDate}{date}
  \item{NoReadingsH}{num, 1}
  \item{RHthreshold}{num, percentage}
}
\details{
Weather and severity
}
\value{
  \item{Wfile}{"Date","Rainfall","Tmp","HumidHrs","humidtmp"}
  \item{Sfile}{"Cultivar","ApplSys","dates","nday","MeanSeverity","StDevSeverity"}
  \item{EmergDate}{date}
  \item{EndEpidDate}{date}
}
\note{
All format data for date is yyyy-mm,dd, for example "2000-04-22".
change with function as.Date()
}

\seealso{\code{\link{lateblight}}}
\examples{
library(agricolae)
f <- system.file("external/weather.csv", package="agricolae")
weather <- read.csv(f,header=FALSE)
f <- system.file("external/severity.csv", package="agricolae")
severity <- read.csv(f)
weather[,1]<-as.Date(weather[,1],format = "\%m/\%d/\%Y")
# Parameters dates and threshold
dates<-c("2000-03-25","2000-04-09","2000-04-12","2000-04-16","2000-04-22")
dates<-as.Date(dates)
EmergDate <- as.Date('2000/01/19')
EndEpidDate <- as.Date("2000-04-22")
dates<-as.Date(dates)
NoReadingsH<- 1
RHthreshold <- 90
#--------------------------
WS<-weatherSeverity(weather,severity,dates,EmergDate,EndEpidDate,
NoReadingsH,RHthreshold)
}
\keyword{ models }
