#' Multiple comparisons, Waller-Duncan
#' 
#' The Waller-Duncan k-ratio t test is performed on all main effect means in
#' the MEANS statement. See the K-RATIO option for information on controlling
#' details of the test.
#' 
#' 
#' It is necessary first makes a analysis of variance.\cr
#' 
#' K-RATIO (K): value specifies the Type 1/Type 2 error seriousness ratio for
#' the Waller-Duncan test. Reasonable values for KRATIO are 50, 100, and 500,
#' which roughly correspond for the two-level case to ALPHA levels of 0.1,
#' 0.05, and 0.01. By default, the procedure uses the default value of 100.\cr
#' 
#' if y = model, then to apply the instruction:\cr waller.test (model, "trt",
#' alpha = 0.05, group = TRUE, main = NULL, console = FALSE)\cr where the model
#' class is aov or lm.
#' 
#' @param y model(aov or lm) or answer of the experimental unit
#' @param trt Constant( only y=model) or vector treatment applied to each unit
#' @param DFerror Degrees of freedom
#' @param MSerror Mean Square Error
#' @param Fc F Value
#' @param K K-RATIO
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
#' \code{\link{friedman}}, \code{\link{HSD.test}}, \code{\link{kruskal}},
#' \code{\link{LSD.test}}, \code{\link{Median.test}}, \code{\link{PBIB.test}},
#' \code{\link{REGW.test}}, \code{\link{scheffe.test}}, \code{\link{SNK.test}},
#' \code{\link{waerden.test}}, \code{\link{plot.group}}
#' @references Waller, R. A. and Duncan, D. B. (1969). A Bayes Rule for the
#' Symmetric Multiple Comparison Problem, Journal of the American Statistical
#' Association 64, pages 1484-1504.\cr
#' 
#' Waller, R. A. and Kemp, K. E. (1976) Computations of Bayesian t-Values for
#' Multiple Comparisons, Journal of Statistical Computation and Simulation, 75,
#' pages 169-172.\cr
#' 
#' Steel & Torry & Dickey. Third Edition 1997 Principles and procedures of
#' statistics a biometrical approach
#' @keywords htest
#' @export
#' @examples
#' 
#' library(agricolae)
#' data(sweetpotato)
#' model<-aov(yield~virus, data=sweetpotato)
#' out <- waller.test(model,"virus", group=TRUE)
#' #startgraph
#' oldpar<-par(mfrow=c(2,2))
#' # variation: SE is error standard
#' # variation: range is Max - Min
#' bar.err(out$means,variation="SD",horiz=TRUE,xlim=c(0,45),bar=FALSE,
#' col=colors()[25],space=2, main="Standard deviation",las=1)
#' bar.err(out$means,variation="SE",horiz=FALSE,ylim=c(0,45),bar=FALSE,
#' col=colors()[15],space=2,main="SE",las=1)
#' bar.err(out$means,variation="range",ylim=c(0,45),bar=FALSE,col="green",
#' space=3,main="Range = Max - Min",las=1)
#' bar.group(out$groups,horiz=FALSE,ylim=c(0,45),density=8,col="red", 
#' main="Groups",las=1)
#' #endgraph
#' # Old version HSD.test()
#' df<-df.residual(model)
#' MSerror<-deviance(model)/df
#' Fc<-anova(model)["virus",4]
#' out <- with(sweetpotato,waller.test(yield, virus, df, MSerror, Fc, group=TRUE))
#' print(out)
#' par(oldpar)
#' 
waller.test <-
  function (y, trt, DFerror, MSerror, Fc, K = 100, group=TRUE,main = NULL,console=FALSE)                             
  {                                                                                                    
    name.y <- paste(deparse(substitute(y)))                                                          
    name.t <- paste(deparse(substitute(trt)))
    if(is.null(main))main<-paste(name.y,"~", name.t)                                                        
    clase<-c("aov","lm")                                                                             
    if("aov"%in%class(y) | "lm"%in%class(y)){                                                        
      if(is.null(main))main<-y$call
      A<-y$model                                                                                       
      DFerror<-df.residual(y)                                                                          
      MSerror<-deviance(y)/DFerror                                                                     
      tabla<-anova(y)                                                                                  
      y<-A[,1]                                                                                         
      ipch<-pmatch(trt,names(A))                                                                       
      nipch<- length(ipch)                                                                             
      for(i in 1:nipch){                                                                           
        if (is.na(ipch[i]))                                                                          
          return(if(console)cat("Name: ", trt, "\n", names(A)[-1], "\n"))                                     
      }                                                                                            
      name.t<- names(A)[ipch][1]                                                                   
      trt <- A[, ipch]                                                                             
      if (nipch > 1){                                                                              
        trt <- A[, ipch[1]]                                                                                
        for(i in 2:nipch){                                                                           
          name.t <- paste(name.t,names(A)[ipch][i],sep=":")                                            
          trt <- paste(trt,A[,ipch[i]],sep=":")                                                            
        }}                                                                                           
      Fc<-tabla[name.t,4]                                                                              
      name.y <- names(A)[1]                                                                            
    }                                                                                                
    junto <- subset(data.frame(y, trt), is.na(y) == FALSE)                                           
    Mean<-mean(junto[,1])
    CV<-sqrt(MSerror)*100/Mean	
    medians<-tapply.stat(junto[,1],junto[,2],stat="median")
    for(i in c(1,5,2:4)) {
      x <- tapply.stat(junto[,1],junto[,2],function(x)quantile(x)[i])
      medians<-cbind(medians,x[,2])
    }
    medians<-medians[,3:7]
    names(medians)<-c("Min","Max","Q25","Q50","Q75")
    means <- tapply.stat(junto[,1],junto[,2],stat="mean") # change                                   
    sds <-   tapply.stat(junto[,1],junto[,2],stat="sd")   # change                                   
    nn <-   tapply.stat(junto[,1],junto[,2],stat="length") # change                                  
    means<-data.frame(means,std=sds[,2],r=nn[,2],medians)
    names(means)[1:2]<-c(name.t,name.y)                                                              
    ntr<-nrow(means)                                                                                 
    Tprob <- waller(K,ntr-1,DFerror,Fc)                                                              
    nr <- unique(nn[,2]) 
    nr1 <-nr                                                                            
    nfila<-c("K ratio", "Error Degrees of Freedom", "Error Mean Square","F value",                       
             "Critical Value of Waller")                                                                          
    nvalor<-c( K,  DFerror, MSerror, Fc, Tprob)                                                          
    if(console){
      cat("\nStudy:", main,"\n\n")                                                                            
      cat("Waller-Duncan K-ratio t Test for",name.y,"\n\n")                                          
      cat("This test minimizes the Bayes risk under additive loss and certain other assumptions\n")                              
    }                                                  
    xtabla<-data.frame("......"=nvalor)                                                                  
    row.names(xtabla)<-nfila                                                                             
    if(console)
      {print(xtabla)                                                                                        
      cat("\n")                                                                                            
      cat(paste(name.t,",",sep="")," means\n\n")                                                           
      print(data.frame(row.names = means[,1], means[,2:6]))
      }                                                 
    MSD <- Tprob * sqrt(2 * MSerror/nr)
    statistics<-data.frame(Mean=Mean,Df=DFerror,CV=CV,MSerror=MSerror,F.Value=Fc)    
    if ( group & length(nr) == 1 & console){
      cat("\nMinimum Significant Difference", MSD)
    }
    if ( group & length(nr) != 1 & console) cat("\nGroups according of treatment differences and k level(",K,")\n")
    if ( length(nr) == 1) statistics<-data.frame(statistics,Waller=Tprob,CriticalDifference=MSD)    

      pvalue<-NULL;alpha<-0                                                         
      comb <-utils::combn(ntr,2)                                                                                  
      nn<-ncol(comb)                                                                                       
      dif<-rep(0,nn)                                                                                       
      MSD1<-rep(0,nn)                                                                                      
      for (k in 1:nn) {                                                                                    
        i<-comb[1,k]                                                                                         
        j<-comb[2,k]                                                                                         
        dif[k]<-means[i,2]-means[j,2]                                                                        
        MSD1[k]<-Tprob*sqrt(MSerror * (1/means[i,4] + 1/means[j,4])) 
        pvalue[k]<-1
        if(abs(dif[k])>MSD1[k])pvalue[k]<-alpha
      } 
      if(!group){  
      tr.i <- means[comb[1, ],1]                                                                           
      tr.j <- means[comb[2, ],1]                                                                           
      if (length(nr1) == 1)  {                                                                              
        significant = abs(dif) > MSD                                                                         
        comparison<-data.frame("Difference" = dif, significant)
      }                                                                                                    
      else  {                                                                                              
        significant = abs(dif) > MSD1                                                                             
        comparison<-data.frame("Difference" = dif, MSD=MSD1,significant)
      }                                                                                                    
      rownames(comparison)<-paste(tr.i,tr.j,sep=" - ")                                                         
      if(console){cat("\nComparison between treatments means\n\n")                                                     
        print(comparison)}                                                                                       
      groups=NULL
      }  
      
   if (group) {
    # Matriz de seudo-probabilidades para segmentar grupos
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
    names(groups)[1]<-name.y
    if(console) {
    cat("\nTreatments with the same letter are not significantly different.\n\n")
    print(groups)
    } 
    comparison<-NULL
  }
    parameters<-data.frame(test="Waller-Duncan",name.t=name.t,ntr = ntr, K=K)
    rownames(parameters)<-" "
    rownames(statistics)<-" "
    rownames(means)<-means[,1]
    means<-means[,-1]
    output<-list(statistics=statistics,parameters=parameters, 
                 means=means,comparison=comparison,groups=groups)
    class(output)<-"group"
    invisible(output)                                                                                    
  }
