\name{audps}
\alias{audps}
%- audpc.
\title{ The Area Under the Disease Progress Stairs }
\description{
   A better estimate of disease progress is the area under the disease progress 
   stairs (AUDPS). The AUDPS approach improves the estimation of disease progress 
   by giving a weight closer to optimal to the first and last observations.
}
\usage{
audps(evaluation, dates, type = "absolute")
}

\arguments{
  \item{evaluation}{ Table of data of the evaluations: Data frame }
  \item{dates}{ Vector of dates corresponding to each evaluation }
  \item{type}{ relative, absolute }
}
\details{
     AUDPS. For the illustration one considers three evaluations
     (14, 21 and 28 days) and percentage of damage in the plant 40, 80 and 90
     (interval between dates of evaluation 7 days).  AUDPS = 1470.
     The evaluations can be at different interval.
     AUDPS= sum( rectangle area by interval in times evaluation ) see example.
}
\value{
Vector with relative or absolute audps.
}
\references{ Ivan Simko, and Hans-Peter Piepho, (2012). The area under the disease 
progress stairs: Calculation, advantage, and application. Phytopathology 102:381- 389.
 }
\author{ Felipe de Mendiburu }

\examples{
library(agricolae)
dates<-c(14,21,28) # days
# example 1: evaluation - vector
evaluation<-c(40,80,90)
audps(evaluation,dates)
audps(evaluation,dates,"relative")
x<-seq(10.5,31.5,7)
y<-c(40,80,90,90)
plot(x,y,"s",ylim=c(0,100),xlim=c(10,32),axes=FALSE,col="red" ,ylab="",xlab="")
title(cex.main=0.8,main="Absolute or Relative AUDPS\nTotal area=(31.5-10.5)*100=2100",
ylab="evaluation",xlab="dates" )
points(x,y,type="h")
z<-c(14,21,28)
points(z,y[-3],col="blue",lty=2,pch=19)
points(z,y[-3],col="blue",lty=2,pch=19)
axis(1,x,pos=0)
axis(2,c(0,40,80,90,100),las=2)
text(dates,evaluation+5,dates,col="blue")
text(14,20,"A = (17.5-10.5)*40",cex=0.8)
text(21,40,"B = (24.5-17.5)*80",cex=0.8)
text(28,60,"C = (31.5-24.5)*90",cex=0.8)
text(14,95,"audps = A+B+C = 1470")
text(14,90,"relative = audps/area = 0.7")
# It calculates audpc absolute
absolute<-audps(evaluation,dates,type="absolute")
print(absolute)
rm(evaluation, dates, absolute)
}
\keyword{ manip }% at least one, from doc/KEYWORDS

