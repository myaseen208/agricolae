#' Finding the Variance Analysis of the Balanced Incomplete Block Design
#' 
#' Analysis of variance BIB and comparison mean adjusted.
#' 
#' Test of comparison treatment.  lsd: Least significant difference. tukey:
#' Honestly significant differente. duncan: Duncan's new multiple range test
#' waller: Waller-Duncan test. snk: Student-Newman-Keuls (SNK)
#' 
#' @param block blocks
#' @param trt Treatment
#' @param y Response
#' @param test Comparison treatments
#' @param alpha Significant test
#' @param group logical
#' @param console logical, print output
#' @return \item{parameters}{Design parameters} \item{statistics}{Statistics of
#' the model} \item{comparison}{Comparison between treatments}
#' \item{means}{Adjusted mean and statistics summary} \item{groups}{Grouping of
#' treatments}
#' @author F. de Mendiburu
#' @seealso \code{\link{DAU.test}}, \code{\link{duncan.test}},
#' \code{\link{durbin.test}}, \code{\link{friedman}}, \code{\link{HSD.test}},
#' \code{\link{kruskal}}, \code{\link{LSD.test}}, \code{\link{Median.test}},
#' \code{\link{PBIB.test}}, \code{\link{REGW.test}},
#' \code{\link{scheffe.test}}, \code{\link{SNK.test}},
#' \code{\link{waerden.test}}, \code{\link{waller.test}},
#' \code{\link{plot.group}}
#' @references Design of Experiments. Robert O. Kuehl. 2nd ed., Duxbury, 2000
#' Linear Estimation and Design of Experiments. D.D. Joshi. WILEY EASTERN
#' LIMITED 1987, New Delhi, India. Introduction to experimental statistics.
#' Ching Chun Li McGraw - Hill Book Company, Inc. New York. 1964
#' @keywords models
#' 
#' @importFrom stats formula sd quantile anova qt qtukey pt ptukey
#' @export
#' @examples
#' 
#' library(agricolae)
#' # Example Design of Experiments. Robert O. Kuehl. 2da. Edicion. 2001
#' run<-gl(10,3)
#' psi<-c(250,325,475,250,475,550,325,400,550,400,475,550,325,475,550,
#' 250,400,475,250,325,400,250,400,550,250,325,550,325,400,475)
#' monovinyl<-c(16,18,32,19,46,45,26,39,61,21,35,55,19,47,48,20,33,31,13,13,34,21,
#'  30,52,24,10,50,24,31,37)
#' out<-BIB.test(run,psi,monovinyl,test="waller",group=FALSE)
#' print(out)
#' bar.err(out$means,variation="range",ylim=c(0,60),bar=FALSE,col=0)
#' out<-BIB.test(run,psi,monovinyl,test="waller",group=TRUE)
#' out<-BIB.test(run,psi,monovinyl,test="tukey",group=TRUE,console=TRUE)
#' out<-BIB.test(run,psi,monovinyl,test="tukey",group=FALSE,console=TRUE)
#' rm(run,psi,monovinyl,out)
#' # Example linear estimation and design of experiments. D.D. Joshi. 1987
#' # Professor of Statistics, Institute of Social Sciences Agra, India
#' # 6 varieties of wheat crop in a BIB whit 10 blocks of 3 plots each.
#' y <-c(69,77,72,63,70,54,65,65,57,59,50,45,68,75,59,38,60,60,62,
#'  55,54,65,62,65,61,39,54,67,63,56)
#' varieties<-gl(6,5)
#' block <- c(1,2,3,4,5,1,2,6,7,8,1,3,6,9,10,2,4,7,9,10,3,5,7,8,9,4,5,6,8,10)
#' BIB.test(block, varieties, y)
#' # Example Introduction to experimental statistics. Ching Chun Li. 1964
#' # pag. 395 table. 27.2
#' # 7 trt, k=3 and b=7.
#' y <-c(10,15,11,4,12,15,5,14,10,14,19,19,8,10,17,6,11,12,5,14,21)
#' block<-gl(7,3)
#' trt <- c(1,2,4,2,3,5,3,4,6,4,5,7,1,5,6,2,6,7,1,3,7)
#' out<-BIB.test(block, trt, y, test="duncan")
#' bar.group(out$groups,col="blue",density=4,ylim=c(0,max(y)))
#' rm(y,block,trt,out)
#' 
BIB.test <-
  function (block, trt, y, test = c("lsd","tukey","duncan","waller","snk"), alpha = 0.05, group = TRUE,console=FALSE)
  {
    test<-match.arg(test)
    block.unadj <- as.factor(block)
    trt.adj <- as.factor(trt)
    name.y <- paste(deparse(substitute(y)))
    name.t <- paste(deparse(substitute(trt)))
    modelo <- formula(paste(name.y,"~ block.unadj + trt.adj"))
    model <- lm(modelo)
    DFerror <- df.residual(model)
    MSerror <- deviance(model)/DFerror
    k <- unique(table(block.unadj))
    r <- unique(table(trt.adj))
    b <- nlevels(block.unadj)
    ntr <- nlevels(trt.adj)
    lambda <- r * (k - 1)/(ntr - 1)
    datos <- data.frame(block, trt, y)
    tabla <- by(datos[,3], datos[,1:2], function(x) mean(x,na.rm=TRUE))
    tabla <-as.data.frame(tabla[,])
    AA <- !is.na(tabla)
    BB <- tapply(y, block.unadj, function(x) sum(x, na.rm=TRUE))
    B <- BB %*% AA
    Y <- tapply(y, trt.adj, function(x) sum(x, na.rm=TRUE))
    mi <- tapply(y, trt.adj, function(x) min(x, na.rm=TRUE))
    ma <- tapply(y, trt.adj, function(x) max(x, na.rm=TRUE))
    sds <- tapply(y, trt.adj, function(x) sd(x, na.rm=TRUE))
    medians<-tapply.stat(y,trt.adj,stat="median")
    for(i in c(1,5,2:4)) {
      x <- tapply.stat(y,trt.adj,function(x)quantile(x)[i])
      medians<-cbind(medians,x[,2])
    }
    medians<-medians[,3:7]
    names(medians)<-c("Min","Max","Q25","Q50","Q75")
    Q <- Y - as.numeric(B)/k
    SStrt.adj <- sum(Q^2) * k/(lambda * ntr)
    MStrt.adj <- SStrt.adj/(ntr - 1)
    sdtdif <- sqrt(2 * k * MSerror/(lambda * ntr))
    Fvalue <- MStrt.adj/MSerror
    mean.adj <- mean(y,na.rm=TRUE) + Q * k/(lambda * ntr)
    StdError.adj <- sqrt(MSerror * (1 + k * r * (ntr - 1)/(lambda *
                                                             ntr))/(r * ntr))
    CV<-cv.model(model)
    Mean<-mean(y,na.rm=TRUE)
    if(console){
      cat("\nANALYSIS BIB: ", name.y, "\nClass level information\n")
      cat("\nBlock: ", unique(as.character(block)))
      cat("\nTrt  : ", unique(as.character(trt)))
      cat("\n\nNumber of observations: ", length(y), "\n\n")
      print(anova(model))
      cat("\ncoefficient of variation:", round(CV, 1),
          "%\n")
      cat(name.y, "Means:", Mean, "\n\n")
      cat(paste(name.t,",",sep="")," statistics\n\n")
    }
    nameTrt<-row.names(Y)
    std <- sds
    means <-data.frame( means=Y/r,mean.adj, SE=StdError.adj,r,std,medians)
    rownames(means)<-nameTrt
    names(means)[1]<-name.y
    if(console)print(means[,1:7])
    parameter <- k/(lambda * ntr)
    snk<-0
    if (test == "lsd") {
      Tprob <- qt(1 - alpha/2, DFerror)
      if(console){
        cat("\nLSD test")
        cat("\nStd.diff   :", sdtdif)
        cat("\nAlpha      :", alpha)
        cat("\nLSD        :", Tprob * sdtdif)}
    }
    if (test == "tukey") {
      Tprob <- qtukey(1 - alpha, ntr, DFerror)
      sdtdif<-sdtdif/sqrt(2)
      if(console){
        cat("\nTukey")
        cat("\nAlpha      :", alpha)
        cat("\nStd.err    :", sdtdif)
        cat("\nHSD        :", Tprob * sdtdif)}
      parameter <- parameter/2
    }
    if (test == "waller") {
      K <- 650 - 16000 * alpha + 1e+05 * alpha^2
      Tprob <- waller(K, ntr - 1, DFerror, Fvalue)
      if(console){
        cat("\nWaller-Duncan K-ratio \n")
        cat("This test minimizes the Bayes risk under additive \n")
        cat("loss and certain other assumptions.\n\n")
        cat("k Ratio    : ", K,"\n")
        cat("MSD        :", Tprob * sdtdif)}
    }
    if (test == "snk") {
      snk<-1
      sdtdif<-sdtdif/sqrt(2)
      Tprob <- qtukey(1-alpha,2:ntr, DFerror)
      SNK <- Tprob * sdtdif
      names(SNK)<-2:ntr
      if(console){cat("\nStudent Newman Keuls")
        cat("\nAlpha     :", alpha)
        cat("\nStd.err   :", sdtdif)
        cat("\nCritical Range\n")
        print(SNK)}
    }
    if (test == "duncan") {
      snk<-2
      sdtdif<-sdtdif/sqrt(2)
      Tprob <- qtukey((1-alpha)^(1:(ntr-1)),2:ntr, DFerror)
      duncan <- Tprob * sdtdif
      names(duncan)<-2:ntr
      if(console){
        cat("\nDuncan's new multiple range test")
        cat("\nAlpha     :", alpha)
        cat("\nStd.err   :", sdtdif)
        cat("\n\nCritical Range\n")
        print(duncan)}
    }
    E <- lambda * ntr/(r * k)
    if(console){
      cat("\nParameters BIB")
      cat("\nLambda     :", lambda)
      cat("\ntreatmeans :", ntr)
      cat("\nBlock size :", k)
      cat("\nBlocks     :", b)
      cat("\nReplication:", r, "\n")
      cat("\nEfficiency factor", E, "\n\n<<< Book >>>\n")}
    parameters<-data.frame(lambda= lambda,treatmeans=ntr,blockSize=k,blocks=b,r=r,alpha=alpha,test="BIB")
    statistics<-data.frame(Mean=Mean,Efficiency=E,CV=CV)
    rownames(parameters)<-" "
    rownames(statistics)<-" "
#
      Omeans<-order(mean.adj,decreasing = TRUE)
      Ordindex<-order(Omeans)
      comb <- utils::combn(ntr, 2)
      nn <- ncol(comb)
      dif <- rep(0, nn)
      sig <- rep(" ",nn)
      pvalue <- dif
      odif<-dif
      significant<-NULL
      for (k in 1:nn) {
        i <- comb[1, k]
        j <- comb[2, k]
        dif[k] <- mean.adj[i] - mean.adj[j]
        if (test == "lsd")
          pvalue[k] <- 2 * round(1 - pt(abs(dif[k])/sdtdif,
                                        DFerror), 4)
        if (test == "tukey")
          pvalue[k] <- round(1 - ptukey(abs(dif[k]) /sdtdif,
                                        ntr, DFerror), 4)
        if (test == "snk"){
          odif[k] <- abs(Ordindex[i]- Ordindex[j])+1
          pvalue[k] <- round(1 - ptukey(abs(dif[k]) /sdtdif,
                                        odif[k], DFerror), 4)
        }
        if (test == "duncan"){
          nx<-abs(i-j)+1
          odif[k] <- abs(Ordindex[i]- Ordindex[j])+1
          pvalue[k]<- round(1-(ptukey(abs(dif[k])/sdtdif,odif[k],DFerror))^(1/(odif[k]-1)),4)
        }
        sig[k]<-" "
        if (pvalue[k] <= 0.001) sig[k]<-"***"
        else  if (pvalue[k] <= 0.01) sig[k]<-"**"
        else  if (pvalue[k] <= 0.05) sig[k]<-"*"
        else  if (pvalue[k] <= 0.1) sig[k]<-"."
        if (test == "waller") {
        significant[k] = abs(dif[k]) > Tprob * sdtdif
        pvalue[k]=1
        if(significant[k]) pvalue[k]=0
        }
        }
      
      tr.i <- nameTrt[comb[1, ]]
      tr.j <- nameTrt[comb[2, ]]
      if(console)cat("\nComparison between treatments means\n")
      if (test == "waller") comparison<-data.frame("Difference" = dif, significant)
      else  comparison<-data.frame("Difference" = dif, pvalue=pvalue,"sig."=sig)
      rownames(comparison)<-paste(tr.i,tr.j,sep=" - ")
      if(console)print(comparison)
      groups <- NULL

#Treatment groups with probabilities 

    if (group) {
      # Matriz de probabilidades para segmentar grupos
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
      groups <- orderPvalue(names(mean.adj), as.numeric(mean.adj),alpha, Q,console)
      names(groups)[1]<-name.y
      if(console) {
        cat("\nTreatments with the same letter are not significantly different.\n\n")
        print(groups) 
      } 
      comparison<-NULL
    }   
 output<-list(parameters=parameters,statistics=statistics,comparison=comparison,means=means,groups=groups)
 class(output)<-"group"
 invisible(output)
  }
