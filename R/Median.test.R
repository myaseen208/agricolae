#' Median test. Multiple comparisons
#' 
#' A nonparametric test for several independent samples. The median test is
#' designed to examine whether several samples came from populations having the
#' same median.
#' 
#' The data consist of k samples of possibly unequal sample size.\cr Greater:
#' is the number of values that exceed the median of all data and \cr
#' LessEqual: is the number less than or equal to the median of all data.
#' 
#' @param y Variable response
#' @param trt Treatments
#' @param alpha error type I
#' @param correct a logical indicating whether to apply continuity correction
#' when computing the test statistic for 2 groups. The correction will not be
#' bigger than the differences themselves. No correction is done if
#' simulate.p.value = TRUE.
#' @param simulate.p.value a logical indicating whether to compute p-values by
#' Monte Carlo simulation
#' @param group TRUE or FALSE
#' @param main Title
#' @param console logical, print output
#' @return \item{statistics}{Statistics of the model} \item{parameters}{Design
#' parameters} \item{medians}{Statistical summary of the study variable}
#' \item{comparison}{Comparison between treatments} \item{groups}{Formation of
#' treatment groups}
#' @author Felipe de Mendiburu
#' @seealso \code{\link{BIB.test}}, \code{\link{DAU.test}},
#' \code{\link{duncan.test}}, \code{\link{durbin.test}},
#' \code{\link{friedman}}, \code{\link{HSD.test}}, \code{\link{kruskal}},
#' \code{\link{LSD.test}}, \code{\link{PBIB.test}}, \code{\link{REGW.test}},
#' \code{\link{scheffe.test}}, \code{\link{SNK.test}},
#' \code{\link{waerden.test}}, \code{\link{waller.test}},
#' \code{\link{plot.group}}
#' @references Practical Nonparametrics Statistics. W.J. Conover, 1999
#' @keywords nonparametric
#' 
#' @importFrom stats median quantile chisq.test
#' @export
#' @examples
#' 
#' library(agricolae)
#' # example 1
#' data(corn)
#' out<-with(corn,Median.test(observation,method,console=FALSE))
#' z<-bar.err(out$medians,variation = "range",ylim=c(0,120),
#'            space=2,border=4,col=3,density=10,angle=45)
#' # example 2
#' out<-with(corn,Median.test(observation,method,console=FALSE,group=FALSE))
#' print(out$comparison)
#' 
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
