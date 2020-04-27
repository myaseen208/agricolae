#' Weather and Severity
#' 
#' Weather and Severity
#' 
#' Weather and severity
#' 
#' @param weather object, see example
#' @param severity object, see example
#' @param dates vector dates
#' @param EmergDate date
#' @param EndEpidDate date
#' @param NoReadingsH num, 1
#' @param RHthreshold num, percentage
#' @return \item{Wfile}{"Date","Rainfall","Tmp","HumidHrs","humidtmp"}
#' \item{Sfile}{"Cultivar","ApplSys","dates","nday","MeanSeverity","StDevSeverity"}
#' \item{EmergDate}{date} \item{EndEpidDate}{date}
#' @note All format data for date is yyyy-mm,dd, for example "2000-04-22".
#' change with function as.Date()
#' @seealso \code{\link{lateblight}}
#' @keywords models
#' @export
#' @examples
#' 
#' library(agricolae)
#' f <- system.file("external/weather.csv", package="agricolae")
#' weather <- read.csv(f,header=FALSE)
#' f <- system.file("external/severity.csv", package="agricolae")
#' severity <- read.csv(f)
#' weather[,1]<-as.Date(weather[,1],format = "%m/%d/%Y")
#' # Parameters dates and threshold
#' dates<-c("2000-03-25","2000-04-09","2000-04-12","2000-04-16","2000-04-22")
#' dates<-as.Date(dates)
#' EmergDate <- as.Date('2000/01/19')
#' EndEpidDate <- as.Date("2000-04-22")
#' dates<-as.Date(dates)
#' NoReadingsH<- 1
#' RHthreshold <- 90
#' #--------------------------
#' WS<-weatherSeverity(weather,severity,dates,EmergDate,EndEpidDate,
#' NoReadingsH,RHthreshold)
#' 
weatherSeverity <-
function(weather,severity,dates,EmergDate,EndEpidDate,NoReadingsH,RHthreshold)
{
nday<- as.numeric(dates-EmergDate)
#........
MeanSeverity<-apply(severity[,4:8],2,function(x) mean(x,na.rm=TRUE))
StDevSeverity<-apply(severity[,4:8],2,function(x)sd(x,na.rm=TRUE))
Sfile<-data.frame(severity[,1:2],dates,nday,MeanSeverity,StDevSeverity)
# ----------------------------
Wfile<-subset(weather,((weather[,1] >= EmergDate) &  (weather[,1] <= EndEpidDate)))
Wfile <- data.frame(Wfile,"","","")
nc<-ncol(Wfile)
for (j in 2:nc) Wfile[,j]<- as.character(Wfile[,j])
for(j in 2:nc) Wfile[Wfile[,j]==".",j]<-""
nr <-nrow(Wfile)
Wfile[,3]<-as.numeric(Wfile[,3])
Wfile[,4]<-as.numeric(Wfile[,4])
Wfile[,5]<-as.numeric(Wfile[,5])
Wfile[,6]<-as.numeric(Wfile[,6])
Wfile[,7]<-as.numeric(Wfile[,7])
Wfile[,8]<-as.numeric(Wfile[,8])

for (i in 1:nr) {
hours<-strsplit(Wfile[i,2], ":", fixed = TRUE)[[1]][1]
if (Wfile[i,2]!="") Wfile[i,6] <- as.numeric(hours)
if (!is.na(Wfile[i,4])){
if (Wfile[i,4] > 100) Wfile[i,4] <- 100
if (Wfile[i,4] > RHthreshold) Wfile[i,7] <- (1 / NoReadingsH)
}
if (!is.na(Wfile[i,7])) {
if (Wfile[i,7] == (1 / NoReadingsH)) Wfile[i,8] <- Wfile[i,3]
}
}
Rainfall <-round(as.matrix(by( Wfile[,5],Wfile[,1],function(x) sum(x,na.rm=TRUE))),1)
tmp <- as.character(row.names(Rainfall))
Date<-as.Date(tmp)
Tmp<- as.matrix(by( Wfile[,3],Wfile[,1],function(x) mean(x,na.rm=TRUE)))
HumidHrs <-as.matrix(by( Wfile[,7],Wfile[,1],function(x) sum(x,na.rm=TRUE)))
humidtmp <- as.matrix(by( Wfile[,8],Wfile[,1],function(x) mean(x,na.rm=TRUE)))
Wfile <- data.frame(Date,Rainfall,Tmp,HumidHrs,humidtmp)
return(list(Wfile=Wfile,Sfile=Sfile,EmergDate=EmergDate,EndEpidDate=EndEpidDate))
}
