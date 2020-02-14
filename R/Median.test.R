Median.test <-
  function(y,trt,alpha=0.05,correct=TRUE,simulate.p.value = FALSE, group = TRUE, main = NULL,console=TRUE){
    name.y <- paste(deparse(substitute(y)))
    name.t <- paste(deparse(substitute(trt)))
    main<-paste(name.y,"~", name.t)
    trt<-as.character(trt)
    trt<-as.factor(trt)
    junto <- subset(data.frame(y, trt), is.na(y) == FALSE)
    M0<-median(y)
    medians<-tapply.stat(junto[,1],junto[,2],median)
    nn <-   tapply.stat(junto[,1],junto[,2],stat="length") # change
    medians<-cbind(medians,r=nn[,2])
    for(i in c(1,5,2,4)) {
      x <- tapply.stat(y,trt,function(x)quantile(x)[i])
      medians<-cbind(medians,x[,2])
    }
    names(medians)<-c(name.t,name.y,"r","Min","Max","Q25","Q75")
    above1<-tapply.stat(y,trt,function(x) sum(x>M0))
    above2<-tapply.stat(y,trt,function(x) sum(x<=M0))
    names(medians)[2]<-"Median"
    ntr<-nrow(medians)
    above<-y>M0
    B<-table(above,trt)
    O<-suppressWarnings(chisq.test(B,correct,simulate.p.value))
    Tall<-O$statistic
    Pall<-O$p.value
    parameter<-O$parameter
    groups<-NULL
    comparison<-NULL
    # All comparison
    comb <-utils::combn(ntr,2)
    nn<-ncol(comb)
    Median<-rep(0,nn)
    Tstat<-rep(0,nn)
    sig<-NULL
    pvalue<-rep(0,nn)
    for (k in 1:nn) {
      i<-comb[1,k]
      j<-comb[2,k]
      ai<-medians[i,1]
      aj<-medians[j,1]
      subgroup<- junto[junto[,2]==ai | junto[,2]==aj,]
      subgroup[,2]<-as.character(subgroup[,2])
      subgroup[,2]<-as.factor(subgroup[,2])
      M<-median(subgroup[,1])
      above<-subgroup[,1]>M
      B<-table(above,subgroup[,2])
      O<-suppressWarnings(chisq.test(B,correct,simulate.p.value))
      pvalue[k]<-O$p.value
      Tstat[k]<-O$statistic
      Median[k]<-M
      sig[k]<-" "
      if (pvalue[k] <= 0.001) sig[k]<-"***"
      else  if (pvalue[k] <= 0.01) sig[k]<-"**"
      else  if (pvalue[k] <= 0.05) sig[k]<-"*"
      else  if (pvalue[k] <= 0.1) sig[k]<-"."
    }
    pvalue<-round(pvalue,4)
    tr.i <- medians[comb[1, ],1]
    tr.j <- medians[comb[2, ],1]
    comparison<-data.frame(median = Median,"chisq"=Tstat, pvalue=pvalue,"signif."=sig)
    rownames(comparison)<-paste(tr.i,tr.j,sep=" and ")
    if(console){
      cat("\nThe Median Test for",main,"\n")
      cat("\nChi Square =", Tall,"  DF =", parameter,"  P.Value", Pall)
      cat("\nMedian =",M0,"\n\n")
      print(data.frame(row.names = medians[, 1], medians[, -1])) 
      cat("\nPost Hoc Analysis\n\n")
      if(!group) {
        print(comparison)
      }}
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
      groups <- orderPvalue(medians[, 1], medians[, 2],alpha, Q,console)
      names(groups)[1]<-name.y
      if(console) {
      cat("Groups according to probability of treatment differences and alpha level.\n")
      cat("\nTreatments with the same letter are not significantly different.\n\n")
      print(groups)
      } 
      comparison<-NULL
    }
    statistics<-data.frame(Chisq=Tall,Df=parameter,p.chisq=Pall,Median=M0)
    parameters<-data.frame(test="Median",name.t=name.t,ntr = ntr,alpha=alpha)
    rownames(parameters)<-" "
    rownames(statistics)<-" "
    rownames(medians)<-medians[,1]
    medians<-medians[,-1]
    output<-list(statistics=statistics,parameters=parameters,medians=medians,
                 comparison=comparison,groups=groups)
    class(output)<-"group"
    invisible(output)
  }
