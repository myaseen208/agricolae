#' Friedman test and multiple comparison of treatments
#' 
#' The data consist of b-blocks mutually independent k-variate random variables
#' Xij, i=1,..,b; j=1,..k. The random variable X is in block i and is
#' associated with treatment j. It makes the multiple comparison of the
#' Friedman test with or without ties. A first result is obtained by
#' friedman.test of R.
#' 
#' The post hoc friedman test is using the criterium Fisher's least significant
#' difference (LSD)
#' 
#' @param judge Identification of the judge in the evaluation
#' @param trt Treatment
#' @param evaluation Variable
#' @param alpha Significant test
#' @param group TRUE or FALSE
#' @param main Title
#' @param console logical, print output
#' @return \item{statistics}{Statistics of the model} \item{parameters}{Design
#' parameters} \item{means}{Statistical summary of the study variable}
#' \item{comparison}{Comparison between treatments} \item{groups}{Formation of
#' treatment groups}
#' @author Felipe de Mendiburu
#' @seealso \code{\link{BIB.test}}, \code{\link{DAU.test}},
#' \code{\link{duncan.test}}, \code{\link{durbin.test}},
#' \code{\link{HSD.test}}, \code{\link{kruskal}}, \code{\link{LSD.test}},
#' \code{\link{Median.test}}, \code{\link{PBIB.test}}, \code{\link{REGW.test}},
#' \code{\link{scheffe.test}}, \code{\link{SNK.test}},
#' \code{\link{waerden.test}}, \code{\link{waller.test}},
#' \code{\link{plot.group}}
#' @references Practical Nonparametrics Statistics. W.J. Conover, 1999
#' @keywords nonparametric
#' @importFrom stats quantile pchisq qt pt
#' @export
#' @examples
#' 
#' library(agricolae)
#' data(grass)
#' out<-with(grass,friedman(judge,trt, evaluation,alpha=0.05, group=TRUE,console=TRUE,
#' main="Data of the book of Conover"))
#' #startgraph
#' plot(out,variation="IQR")
#' #endgraph
#' 
friedman <-
  function(judge,trt,evaluation,alpha=0.05,group=TRUE,main=NULL,console=FALSE){
    name.x <- paste(deparse(substitute(judge)))
    name.y <- paste(deparse(substitute(evaluation)))
    name.t <- paste(deparse(substitute(trt)))
    name.j <- paste(deparse(substitute(judge)))
    if(is.null(main))main<-paste(name.y,"~", name.j,"+",name.t)
    datos <- data.frame(judge, trt, evaluation)
    matriz <- by(datos[,3], datos[,1:2], function(x) mean(x,na.rm=TRUE))
    matriz <-as.data.frame(matriz[,])
    #matriz <-as.matrix(evaluation)
    name<-as.character(colnames(matriz))
    ntr <-length(name)
    m<-dim(matriz)
    v<-array(0,m)
    for (i in 1:m[1]){
      v[i,]<-rank(matriz[i,])
    }
    vv<-as.numeric(v)
    junto <- data.frame(evaluation, trt)
    medians<-tapply.stat(junto[,1],junto[,2],stat="median")
    for(i in c(1,5,2:4)) {
      x <- tapply.stat(junto[,1],junto[,2],function(x)quantile(x)[i])
      medians<-cbind(medians,x[,2])
    }
    medians<-medians[,3:7]
    names(medians)<-c("Min","Max","Q25","Q50","Q75")
    Means <- tapply.stat(junto[,1],junto[,2],stat="mean")  # change
    sds <-   tapply.stat(junto[,1],junto[,2],stat="sd")    # change
    nn <-   tapply.stat(junto[,1],junto[,2],stat="length") # change
    nr<-unique(nn[,2])
    s<-array(0,m[2])
    # Suma de rangos por tratamiento
    for (j in 1:m[2]){
      s[j]<-sum(v[,j])
    }
    Means<-data.frame(Means,std=sds[,2],r=nn[,2],medians) 
    names(Means)[1:2]<-c(name.t,name.y)
    means<-Means[,c(1:2,4)]
    rownames(Means)<-Means[,1]
    Means<-Means[,-1]
    means[,2]<-s
    # row.names(means)<-means[,1]
    rs<-array(0,m[2])
    rs<-s-m[1]*(m[2]+1)/2
    T1<-12*t(rs)%*%rs/(m[1]*m[2]*(m[2]+1))
    T2<-(m[1]-1)*T1/(m[1]*(m[2]-1)-T1)
    # Impresion de resultados
    if(console){
      cat("\nStudy:",main,"\n\n")
      cat(paste(name.t,",",sep="")," Sum of the ranks\n\n")
      print(data.frame(row.names = means[,1], means[,-1]))
      cat("\nFriedman's Test")
      cat("\n===============")
    }
    A1<-0
    for (i in 1:m[1]) A1 <- A1 + t(v[i,])%*%v[i,]
    DFerror <-(m[1]-1)*(m[2]-1)
    Tprob<-qt(1-alpha/2,DFerror)
    #
    LSD<-as.numeric(Tprob*sqrt(2*(m[1]*A1-t(s)%*%s)/DFerror))
    
    C1 <-m[1]*m[2]*(m[2]+1)^2/4
    T1.aj <-(m[2]-1)*(t(s)%*%s-m[1]*C1)/(A1-C1)
    T2.aj <-(m[1]-1)*T1.aj/(m[1]*(m[2]-1)-T1.aj)
    p.value<-1-pchisq(T1.aj,m[2]-1)
    p.noadj<-1-pchisq(T1,m[2]-1)
    PF<-1-pf(T2.aj, ntr-1, (ntr-1)*(nr-1) )
    if(console){
      cat("\nAdjusted for ties")
      cat("\nCritical Value:",T1.aj)
      cat("\nP.Value Chisq:",p.value)
      cat("\nF Value:",T2.aj)
      cat("\nP.Value F:",PF,"\n")
      cat("\nPost Hoc Analysis\n")
    }
    #...............
    statistics<-data.frame(Chisq=T1.aj,Df=ntr-1,p.chisq=p.value,F=T2.aj,DFerror=DFerror,p.F=PF,t.value=Tprob,LSD)
    if ( group & length(nr) == 1 & console){
      cat("\nAlpha:",alpha,"; DF Error:",DFerror)
      cat("\nt-Student:",Tprob)
      cat("\nLSD:", LSD,"\n")
    }
    if ( group & length(nr) != 1 & console) cat("\nGroups according to probability of treatment differences and alpha level(",alpha,")\n")
    if ( length(nr) != 1) statistics<-data.frame(Chisq=T1.aj,Df=ntr-1,p.chisq=p.value,F=T2.aj,DFerror=DFerror,p.F=PF) 
    comb <-utils::combn(ntr,2)
    nn<-ncol(comb)
    dif<-rep(0,nn)
    pvalue<-rep(0,nn)
    LCL<-dif
    UCL<-dif
    sig<-NULL
    LSD<-rep(0,nn)
    stat<-rep("ns",nn)
    for (k in 1:nn) {
      i<-comb[1,k]
      j<-comb[2,k]
      dif[k]<-s[comb[1,k]]-s[comb[2,k]]
      sdtdif<- sqrt(2*(m[1]*A1-t(s)%*%s)/DFerror)
      pvalue[k]<- round(2*(1-pt(abs(dif[k])/sdtdif,DFerror)),4)
      LSD[k]<-round(Tprob*sdtdif,2)
      LCL[k] <- dif[k] - LSD[k]
      UCL[k] <- dif[k] + LSD[k]
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
      if(console){cat("\nComparison between treatments\nSum of the ranks\n\n")
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
      names(groups)[1]<-"Sum of ranks"
      if(console) {
      cat("\nTreatments with the same letter are not significantly different.\n\n")
      print(groups)
      } 
      comparison<-NULL
    }
    parameters<-data.frame(test="Friedman",name.t=name.t,ntr = ntr,alpha=alpha)
    rownames(parameters)<-" "
    rownames(statistics)<-" "
    Means<-data.frame(rankSum=means[,2],Means)
    Means<-Means[,c(2,1,3:9)]
    output<-list(statistics=statistics,parameters=parameters, 
                 means=Means,comparison=comparison,groups=groups)
    class(output)<-"group"
    invisible(output)
  }
