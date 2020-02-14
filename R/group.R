plot.group<-function(x,variation=c("range","IQR","SE","SD"), horiz=FALSE,
                     col=NULL,xlim=NULL,ylim=NULL,main=NULL,cex=NULL,hy=0,...){
  
  if(class(x) != "group"){
    stop("'x' must be of class 'group'.")
  }
   variation<-match.arg(variation)
  if("means" %in% names(x)) z<-x$means
  else z<-x$medians
  z<-z[rownames(x$groups),]
  if("SE" %in% names(z)){
  cat("\nWarning  values plot is not adjusted\n") 
 }
  y<-z[,1]
  names(y)<-rownames(z)
  groups<-x$groups[,2]
  n<-length(y)
  colores<-as.numeric(groups)
  if(is.null(col)) {
    seqcol<-c(30,34,51,24,654,31,48,6,12,586,137,12,53,387,19,22,23,401, 
              430,115,413,417,420,423,425,428,450,453,455,459,460,465,471,474,477, 
              483,488,496,502,507,512,518,523,549,552,556,562,568,576,578,134,599, 
              602,609,614,619,624,630,638,640,641,645,650,434,657)
    col<-colors()[seqcol[colores]]
  }
  else col<-col[colores]

  # variation types
  if( variation=="range" ) {
    nivel0<-z$"Min"
    nivel1<-z$"Max"
    title<-"Range"
  }
  if( variation=="IQR") {
    nivel0<-z$"Q25"
    nivel1<-z$"Q75"
    title<-"Interquartile range"
  }
  if( variation=="SD" ) {
  if("std" %in% names(z)){
    nivel0<-y-z$"std"
    nivel1<-y+z$"std"
    title<-"Standard deviation"
    }
    else return("For variation use IQR or range")
  }  
  if( variation=="SE" ) {
  	if("std" %in% names(z)){
        std.err<-z$"std"/sqrt(z$"r") 
        nivel0<-y-std.err
  	nivel1<-y+std.err
  	title<-"Standard error"
  	}
  	else return("For variation use IQR or range")
  }	
  if(is.null(main))main=paste("Groups and",title)
  
  top<-1.2*max(nivel1)
  bottom<-0.8*min(nivel0)
  if(horiz) {
    if(is.null(xlim)) xlim<-c(bottom,top)
  }
    else {
      if(is.null(ylim)) ylim<-c(bottom,top)
    }
    
  indice<-barplot(y,horiz=horiz,border=0,col=0,,main=main,xlim=xlim,ylim=ylim,...)
  
  mas<-0.05*max(nivel1)+hy
  if (!horiz)  {
    points(indice,y,pch=19,cex=0.7,col=col)
    text(indice,nivel1+mas,groups,col=col,cex=cex)
    axis(1,as.numeric(indice),labels = FALSE)
  }
  else {
    points(y,indice,pch=19,cex=0.7,col=col)
    text(nivel1+mas,indice,groups,col=col,cex=cex)
    axis(2,as.numeric(indice),labels = FALSE)
  }
  for ( i in 1:n) {
    if (horiz)  {
      lines(rbind(c(nivel0[i],indice[i]),c(nivel1[i],indice[i])),col=col[i])
    }
    else {
      lines(rbind(c(indice[i],nivel0[i]),c(indice[i],nivel1[i])),col=col[i])
    }
  } 
  invisible(indice)
}
