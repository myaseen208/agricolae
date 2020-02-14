bar.err <-
function(x,variation=c("SE","SD","range","IQR"),horiz=FALSE, bar=TRUE,...) {
variation<-match.arg(variation)
y<-x[,1]
names(y)<-rownames(x)
if( variation=="SE" ) {
if( "std"%in%names(x) & variation != "IQR") {
std.err<-x$"std"/sqrt(x$"r")
nivel0<-y-std.err
nivel1<-y+std.err
}
else return("For variation use IQR or range")
}
if( variation=="SD" ) {
if("std" %in% names(x)){
nivel0<-y-x$"std"
nivel1<-y+x$"std"
}
else return("For variation use IQR or range")
}
if( variation=="range" ) {
nivel0<-x$"Min"
nivel1<-x$"Max"
}
if( variation=="IQR") {
nivel0<-x$"Q25"
nivel1<-x$"Q75"
}

n<-length(y)
if (bar) {
indice<-barplot(y,horiz=horiz, ...)
tope<-max(nivel1)/20
}
else {
indice<-barplot(y,horiz=horiz, border=0, ...)
if(horiz)lines(y,indice,type="b")
else lines(indice,y,type="b")
}
for ( i in 1:n) {
if (horiz)  {
lines(rbind(c(nivel0[i],indice[i]),c(nivel1[i],indice[i])))
text( cex=1,nivel0[i],indice[i],"[")
text( cex=1,nivel1[i],indice[i],"]")
}
else {
lines(rbind(c(indice[i],nivel0[i]),c(indice[i],nivel1[i])))
text( cex=1,indice[i],nivel0[i],"---")
text( cex=1,indice[i],nivel1[i],"---")
}
}
invisible(list(x=indice,height=y))
}
