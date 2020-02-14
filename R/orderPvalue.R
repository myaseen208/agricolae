orderPvalue <-
function (treatment, means, alpha, pvalue, console) 
{
  n <- length(means)
  z <- data.frame(treatment, means)
  letras<-c(letters[1:26],LETTERS[1:26],1:9,c(".","+","-","*","/","#","$",
"%","&","^","[","]",":","@",";","_","?","!","=","#",rep(" ",2000)))
  w <- z[order(z[, 2], decreasing = TRUE), ]
  M<-rep("",n)
  k<-1
  k1<-0
  j<-1
  i<-1
  cambio<-n
  cambio1<-0
  chequeo=0
  M[1]<-letras[k]
  q <- as.numeric(rownames(w)) #Check
  while(j<n) {
    chequeo<-chequeo+1
    if (chequeo > n) break
    for(i in j:n) {
      s<-pvalue[q[i],q[j]]>alpha
      if(s) {
        if(lastC(M[i]) != letras[k])M[i]<-paste(M[i],letras[k],sep="")
      }
      else {
        k<-k+1
        cambio<-i
        cambio1<-0
        ja<-j
        for(jj in cambio:n) M[jj]<-paste(M[jj],"",sep="") # El espacio
        M[cambio]<-paste(M[cambio],letras[k],sep="")
        for( v in ja:cambio) {
          if(pvalue[q[v],q[cambio]]<=alpha) {j<-j+1
          cambio1<-1
          }
          else break
        }
        break
      }
    }
    if (cambio1 ==0 )j<-j+1
  }
  #-----------
  w<-data.frame(w,stat=M)
  trt <- as.character(w$treatment)
  means <- as.numeric(w$means)
  output <- data.frame(means, groups=M)
  rownames(output)<-trt
  if(k>81) 
    cat("\n",k,"groups are estimated.The number of groups exceeded the maximum of 81 labels. change to group=FALSE.\n")
    invisible(output)
}
