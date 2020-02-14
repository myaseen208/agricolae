bar.group <-
function(x,horiz=FALSE, ...) {
y<-x[,1]
names(y)<-rownames(x)
nivel<-x[,2]
n<-length(y)
index<-barplot(y,horiz=horiz, ...)
tope<-max(y)/10
for ( i in 1:n) {
if (horiz) text(y[i]+tope,index[i],nivel[i])
else {
if (y[i]<0) text(index[i],abs(tope),nivel[i])
else text(index[i],y[i]+tope,nivel[i])
}
}
invisible(list(x=index,height=y))
}
