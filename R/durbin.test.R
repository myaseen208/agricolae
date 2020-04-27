#' Durbin test and multiple comparison of treatments
#' 
#' A multiple comparison of the Durbin test for the balanced incomplete blocks
#' for sensorial or categorical evaluation. It forms groups according to the
#' demanded ones for level of significance (alpha); by default, 0.05.
#' 
#' The post hoc test is using the criterium Fisher's least significant
#' difference.
#' 
#' @param judge Identification of the judge in the evaluation
#' @param trt Treatments
#' @param evaluation variable
#' @param alpha level of significant
#' @param group TRUE or FALSE
#' @param main Title
#' @param console logical, print output
#' @return \item{statistics}{Statistics of the model} \item{parameters}{Design
#' parameters} \item{means}{Statistical summary of the study variable}
#' \item{rank}{rank table of the study variable} \item{comparison}{Comparison
#' between treatments} \item{groups}{Formation of treatment groups}
#' @author Felipe de Mendiburu
#' @seealso \code{\link{BIB.test}}, \code{\link{DAU.test}},
#' \code{\link{duncan.test}}, \code{\link{friedman}}, \code{\link{HSD.test}},
#' \code{\link{kruskal}}, \code{\link{LSD.test}}, \code{\link{Median.test}},
#' \code{\link{PBIB.test}}, \code{\link{REGW.test}},
#' \code{\link{scheffe.test}}, \code{\link{SNK.test}},
#' \code{\link{waerden.test}}, \code{\link{waller.test}},
#' \code{\link{plot.group}}
#' @references Practical Nonparametrics Statistics. W.J. Conover, 1999
#' Nonparametric Statistical Methods. Myles Hollander and Douglas A. Wofe, 1999
#' @keywords nonparametric
#' @importFrom stats quantile pchisq qt pt
#' @export
#' @examples
#' 
#' library(agricolae)
#' # Example 1. Conover, pag 391
#' person<-gl(7,3)
#' variety<-c(1,2,4,2,3,5,3,4,6,4,5,7,1,5,6,2,6,7,1,3,7)
#' preference<-c(2,3,1,3,1,2,2,1,3,1,2,3,3,1,2,3,1,2,3,1,2)
#' out<-durbin.test(person,variety,preference,group=TRUE,console=TRUE,
#' main="Seven varieties of ice cream manufacturer")
#' #startgraph
#' bar.group(out$groups,horiz=TRUE,xlim=c(0,10),density=4,las=1)
#' #endgraph
#' # Example 2. Myles Hollander, pag 311
#' # Source: W. Moore and C.I. Bliss. 1942
#' day<-gl(7,3)
#' chemical<-c("A","B","D","A","C","E","C","D","G","A","F","G","B","C","F",
#'  "B","E","G","D","E","F")
#' toxic<-c(0.465,0.343,0.396,0.602,0.873,0.634,0.875,0.325,0.330,0.423,0.987,
#' 0.426,0.652,1.142,0.989,0.536,0.409,0.309,0.609,0.417,0.931)
#' out<-durbin.test(day,chemical,toxic,group=TRUE,console=TRUE,
#' main="Logarithm of Toxic Dosages")
#' plot(out)
#' 
durbin.test <-
  function(judge,trt,evaluation,alpha=0.05, group=TRUE,main=NULL,console=FALSE) {
    name.y <- paste(deparse(substitute(evaluation)))
    name.t <- paste(deparse(substitute(trt)))
    name.j <- paste(deparse(substitute(judge)))
    if(is.null(main))main<-paste(name.y,"~", name.j,"+",name.t)
    judge<-as.factor(judge)
    trt<-as.factor(trt)
    k <-unique(table(judge))
    r <-unique(table(trt))
    b <-nlevels(judge)
    ntr <-nlevels(trt)
    lambda<-r*(k-1)/(ntr-1)
    x<-data.frame(judge,trt,evaluation)
    medians<-tapply.stat(x[,3],x[,2],stat="median")
    for(i in c(1,5,2:4)) {
    z <- tapply.stat(x[,3],x[,2],function(x)quantile(x)[i])
    medians<-cbind(medians,z[,2])
    }
    medians<-medians[,3:7]
    names(medians)<-c("Min","Max","Q25","Q50","Q75")
    Means<- tapply.stat(x[,3],x[,2],stat="mean")  # change
    sds <-  tapply.stat(x[,3],x[,2],stat="sd")
    mi <-   tapply.stat(x[,3],x[,2],stat="min")
    ma <-   tapply.stat(x[,3],x[,2],stat="max")
    Means<-data.frame(Means,std=sds[,2],r,medians)
    rownames(Means)<-Means[,1]
    Means<-Means[,-1]
    names(Means)[1] <- name.y
    # Determina el rango dentro de cada juez
    z <- by(x,x$judge,function(x) rank(x$evaluation))
    y<-data.frame(c(z))
    m<-dim(y)
    n<-m[1]*m[2]
    rango <- 1:n
    for (i in 1:m[1]) {
      for (j in 1:m[2]) {
        kk=i+m[1]*(j-1)
        rango[kk]<-y[i,j]
      }
    }
    x <- data.frame(x, rango)
    means <- tapply.stat(x[, 4], x[, 2], stat = "sum")
    names(means)[1:2] <- c(name.t, name.y)
    x<-data.frame(x,rango)
    
    names(means)[1:2]<-c(name.t,name.y)
    z <-by(x,x$trt,function(x) sum(x$rango))
    y<-as.vector(c(z))
    name<-as.character(dimnames(z)$"x$trt")
    s <- (y-r*(k+1)/2)^2
    s1 <- sum(s)
    # determina el valor de Durbin
    gl1<-ntr-1 ;gl2<-b*k-ntr-b+1
    C <- b*k*(k+1)^2/4
    A <- sum(rango^2)
    s <- (ntr - 1) * s1/(A-C)
    prob<-1-pchisq(s,gl1); Tprob<-qt(1-alpha/2,gl2)
    sdtdif <- sqrt(2*r*(A-C)*(1-s/(b*(k-1)))/gl2)
    LSD <-Tprob*sdtdif
    nameTrt<-as.character(means[,1])
    # s,prob,Tprob,Mc,gl1,gl2)
    # Impresion de resultados
    if(console){
      cat("\nStudy:",main,"\n")
      cat(paste(name.t,",",sep="")," Sum of ranks\n\n")
      print(data.frame(row.names = nameTrt, sum=means[,2]))
      cat("\nDurbin Test")
      cat("\n===========")
      cat("\nValue      :",s)
      cat("\nDF 1       :",gl1)
      cat("\nP-value    :",prob)
      cat("\nAlpha      :",alpha)
      cat("\nDF 2       :",gl2)
      cat("\nt-Student  :",Tprob)
      cat("\n\nLeast Significant Difference\nbetween the sum of ranks: ",LSD,"\n")
      # comparacion de tratamientos.
      cat("\nParameters BIB")
      cat("\nLambda     :",lambda)
      cat("\nTreatmeans :",ntr)
      cat("\nBlock size :",k)
      cat("\nBlocks     :",b)
      cat("\nReplication:",r,"\n")
    }

      comb <-utils::combn(ntr,2)
      nn<-ncol(comb)
      dif<-rep(0,nn)
      pvalue<-rep(0,nn)
      sig<-rep(" ",nn)
      for (kk in 1:nn) {
        i<-comb[1,kk]
        j<-comb[2,kk]
        dif[kk]<-y[comb[1,kk]]-y[comb[2,kk]]
        pvalue[kk]<- 2*round(1-pt(abs(dif[kk])/sdtdif,gl2),4)
        sig[kk]<-" "
        if (pvalue[kk] <= 0.001) sig[kk]<-"***"
        else  if (pvalue[kk] <= 0.01) sig[kk]<-"**"
        else  if (pvalue[kk] <= 0.05) sig[kk]<-"*"
        else  if (pvalue[kk] <= 0.1) sig[kk]<-"."
      }

    if(!group){  
      tr.i <- means[comb[1, ],1]
      tr.j <- means[comb[2, ],1]
      comparison<-data.frame("difference" = dif, pvalue=pvalue,"signif."=sig)
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
    #
    parameters<-data.frame(test="Durbin",name.t=name.t,treatments=ntr,blockSize=k,blocks=b,r=r,lambda=lambda,alpha=alpha)
    statistics<-data.frame(chisq.value=s, p.value=prob, t.value=Tprob,LSD=LSD)
    rownames(parameters)<-" "
    rownames(statistics)<-" "
    output<-list(statistics=statistics,parameters=parameters, 
                 means=Means,rank=means,comparison=comparison,groups=groups)
    class(output)<-"group"
    invisible(output)
  }
