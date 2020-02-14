waerden.test <-
  function(y, trt,alpha=0.05,group=TRUE,main=NULL,console=FALSE) {
    name.y <- paste(deparse(substitute(y)))
    name.t <- paste(deparse(substitute(trt)))
    if(is.null(main))main<-paste(name.y,"~", name.t)
    junto <- subset(data.frame(y, trt), is.na(y) == FALSE)
    medians<-tapply.stat(junto[,1],junto[,2],stat="median")
    for(i in c(1,5,2:4)) {
      x <- tapply.stat(junto[,1],junto[,2],function(x)quantile(x)[i])
      medians<-cbind(medians,x[,2])
    }
    medians<-medians[,3:7]
    names(medians)<-c("Min","Max","Q25","Q50","Q75")
    Means <- tapply.stat(junto[,1],junto[,2],stat="mean") # change
    sds <-   tapply.stat(junto[,1],junto[,2], stat="sd")  #change
    nn <-   tapply.stat(junto[,1],junto[,2],stat="length") # change
    Means<-data.frame(Means,std=sds[,2],r=nn[,2],medians) 
    rownames(Means)<-Means[,1]
    Means<-Means[,-1]
    names(Means)[1]<-name.y
    N<- nrow(junto)
    junto[, 1] <- qnorm(round(rank(junto[, 1]) /(N+1),3))
    S <- sum(junto[,1]^2)/(N-1)
    means <- tapply.stat(junto[,1],junto[,2],stat="mean") # change
    sds <-   tapply.stat(junto[,1],junto[,2], stat="sd")  #change
    nn <-   tapply.stat(junto[,1],junto[,2],stat="length") # change
    means<-data.frame(means,std=sds[,2],r=nn[,2])  
    names(means)[1:2]<-c(name.t,name.y)
    #row.names(means)<-means[,1]
    ntr<-nrow(means)
    DFerror<-N - ntr
    T1 <- 0
    for (i in 1:ntr) {
      T1 <- T1 + means[i, 2]^2*means[i,4] # change
    }
    T1<-T1/S
    p.chisq <- 1 - pchisq(T1, ntr - 1)
    if(console){
      cat("\nStudy:",main)
      cat("\nVan der Waerden (Normal Scores) test's\n")
      cat("\nValue :", T1)
      cat("\nPvalue:", p.chisq)
      cat("\nDegrees of Freedom: ", ntr - 1,"\n\n")
      cat(paste(name.t,",",sep="")," means of the normal score\n\n")
      print(data.frame(row.names = means[,1], means[,-1]))
      cat("\nPost Hoc Analysis\n") 
    }
    MSerror <- S * ((N - 1 - T1)/(N - ntr))
    #...............
    nr <- unique(means[,4]) # change
    nr1<-nr
    Tprob<-qt(1-alpha/2,DFerror)
    LSD <- Tprob * sqrt(2 * MSerror/nr)    
    statistics<-data.frame(Chisq=T1,Df=ntr-1,p.chisq=p.chisq )   
    if ( group & length(nr) == 1 & console){
      cat("\nAlpha:",alpha,"; DF Error:",DFerror,"\n")
      cat("\nMinimum Significant Difference:", LSD,"\n")
    }
    if ( group & length(nr) != 1 & console) cat("\nGroups according to probability of treatment differences and alpha level(",alpha,")\n")
    if ( length(nr) == 1) statistics<-data.frame(statistics,t.value=Tprob,MSD=LSD)    
      comb <-utils::combn(ntr,2)
      nn<-ncol(comb)
      dif<-rep(0,nn)
      LCL<-dif
      UCL<-dif
      sig<-NULL
      pvalue<-rep(0,nn)
      for (k in 1:nn) {
        i<-comb[1,k]
        j<-comb[2,k]
        dif[k]<-means[i,2]-means[j,2]
        sdtdif<- sqrt(S*((N-1-T1)/(N-ntr))*(1/means[i,4]+1/means[j,4])) # change
        pvalue[k]<- round(2*(1-pt(abs(dif[k])/sdtdif,DFerror)),4)
        LSD <- Tprob*sdtdif
        LCL[k] <- dif[k] - LSD
        UCL[k] <- dif[k] + LSD
        sig[k]<-" "
        if (pvalue[k] <= 0.001) sig[k]<-"***"
        else  if (pvalue[k] <= 0.01) sig[k]<-"**"
        else  if (pvalue[k] <= 0.05) sig[k]<-"*"
        else  if (pvalue[k] <= 0.1) sig[k]<-"."
      }
    if(!group){  
      tr.i <- means[comb[1, ],1]
      tr.j <- means[comb[2, ],1]
      comparison<-data.frame("difference" = dif, pvalue=pvalue,"signif."=sig,LCL,UCL)
      rownames(comparison)<-paste(tr.i,tr.j,sep=" - ")
    if(console){cat("\nComparison between treatments\nmean of the normal score\n\n")
        print(comparison)}
      groups=NULL
    }
    if (group) {
      # Matriz de probabilidades
      Q<-matrix(1,ncol=ntr,nrow=ntr)
      p<-pvalue
      k<-0
      for(i in 1:(ntr-1)){
        for(j in (i+1):ntr){
          k<-k+1
          Q[i,j]<-p[k]
          Q[j,i]<-p[k]
        }
      }
      groups <- orderPvalue(means[, 1], means[, 2],alpha, Q,console)
      names(groups)[1]<-"score"
      if(console) {
      cat("\nTreatments with the same letter are not significantly different.\n")
      cat("\nMeans of the normal score\n\n")
      print(groups)
      }       
      comparison<-NULL
    }
    Means<-data.frame(normalScore=means[,2],Means)
    Means<-Means[,c(2,1,3:9)]
    parameters<-data.frame(test="Waerden",name.t=name.t,ntr = ntr, alpha=alpha)
    rownames(parameters)<-" "
    rownames(statistics)<-" "
    output<-list(statistics=statistics,parameters=parameters, 
                 means=Means,comparison=comparison,groups=groups)
    class(output)<-"group"
    invisible(output)
  }
 