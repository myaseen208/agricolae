LSD.test <-
  function (y, trt, DFerror, MSerror, alpha = 0.05, p.adj = c("none","holm","hommel",
  "hochberg", "bonferroni", "BH", "BY", "fdr"), group = TRUE, main = NULL,console=FALSE)
  {
    p.adj <- match.arg(p.adj)
    clase <- c("aov", "lm")
    name.y <- paste(deparse(substitute(y)))
    name.t <- paste(deparse(substitute(trt)))
    if(is.null(main))main<-paste(name.y,"~", name.t)
    if ("aov" %in% class(y) | "lm" %in% class(y)) {
      if(is.null(main))main<-y$call
      A <- y$model
      DFerror <- df.residual(y)
      MSerror <- deviance(y)/DFerror
      y <- A[, 1]
      ipch <- pmatch(trt, names(A))
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
    means <- tapply.stat(junto[, 1], junto[, 2], stat = "mean")
    sds <- tapply.stat(junto[, 1], junto[, 2], stat = "sd")
    nn <- tapply.stat(junto[, 1], junto[, 2], stat = "length")
    std.err <- sqrt(MSerror)/sqrt(nn[, 2]) # change sds[,2]
    Tprob <- qt(1 - alpha/2, DFerror)
    LCL <- means[, 2] - Tprob * std.err
    UCL <- means[, 2] + Tprob * std.err
    means <- data.frame(means, std=sds[,2], r = nn[, 2],
                        LCL, UCL,medians)
    names(means)[1:2] <- c(name.t, name.y)
    ntr <- nrow(means)
    nk <- choose(ntr, 2)
    if (p.adj != "none") {
      a <- 1e-06
      b <- 1
      for (i in 1:100) {
        x <- (b + a)/2
        xr <- rep(x, nk)
        d <- p.adjust(xr, p.adj)[1] - alpha
        ar <- rep(a, nk)
        fa <- p.adjust(ar, p.adj)[1] - alpha
        if (d * fa < 0)
          b <- x
        if (d * fa > 0)
          a <- x
      }
      Tprob <- qt(1 - x/2, DFerror)
    }
    nr <- unique(nn[, 2])
    if(console){
      cat("\nStudy:", main)
      if(console)cat("\n\nLSD t Test for", name.y, "\n")
      if (p.adj != "none")cat("P value adjustment method:", p.adj, "\n")
      cat("\nMean Square Error: ", MSerror, "\n\n")
      cat(paste(name.t, ",", sep = ""), " means and individual (",
          (1 - alpha) * 100, "%) CI\n\n")
      print(data.frame(row.names = means[, 1], means[, 2:8]))
      cat("\nAlpha:", alpha, "; DF Error:", DFerror)
      cat("\nCritical Value of t:", Tprob, "\n")
    }
    statistics<-data.frame(MSerror=MSerror,Df=DFerror,Mean=Mean,CV=CV)     
if (length(nr) == 1)  LSD <- Tprob * sqrt(2 * MSerror/nr)
if ( group & length(nr) == 1 & console) {
 if(p.adj=="none") cat("\nleast Significant Difference:",LSD,"\n")
 else cat("\nMinimum Significant Difference:",LSD,"\n")
}
if ( group & length(nr) != 1 & console) 
cat("\nGroups according to probability of means differences and alpha level(",alpha,")\n")

if ( length(nr) == 1 & p.adj=="none") statistics<-data.frame(statistics, t.value=Tprob,LSD=LSD) 
if ( length(nr) == 1 & p.adj!="none") statistics<-data.frame(statistics, t.value=Tprob,MSD=LSD)    
    LSD=" "
    comb <- utils::combn(ntr, 2)
    nn <- ncol(comb)
    dif <- rep(0, nn)
    pvalue <- dif
    sdtdif <- dif
    sig <- rep(" ", nn)
    for (k in 1:nn) {
      i <- comb[1, k]
      j <- comb[2, k]

      dif[k] <-means[i, 2] - means[j, 2]
      sdtdif[k] <- sqrt(MSerror * (1/means[i, 4] + 1/means[j,4]))
      pvalue[k] <- 2 * (1 - pt(abs(dif[k])/sdtdif[k], DFerror))
    }
    if (p.adj != "none")
      pvalue <- p.adjust(pvalue, p.adj)
    pvalue <- round(pvalue,4)
    for (k in 1:nn) {
      if (pvalue[k] <= 0.001)
        sig[k] <- "***"
      else if (pvalue[k] <= 0.01)
        sig[k] <- "**"
      else if (pvalue[k] <= 0.05)
        sig[k] <- "*"
      else if (pvalue[k] <= 0.1)
        sig[k] <- "."
    }
    tr.i <- means[comb[1, ], 1]
    tr.j <- means[comb[2, ], 1]
    LCL <- dif - Tprob * sdtdif
    UCL <- dif + Tprob * sdtdif
    comparison <- data.frame(difference = dif, pvalue = pvalue, "signif."=sig, LCL, UCL)
    if (p.adj !="bonferroni" & p.adj !="none"){
      comparison<-comparison[,1:3]
  #    statistics<-statistics[,1:4]
    }
    rownames(comparison) <- paste(tr.i, tr.j, sep = " - ")
    if (!group) {
      if(console){
        cat("\nComparison between treatments means\n\n")
        print(comparison)
      }
      groups <- NULL
   #   statistics<-statistics[,1:4]
    }
    if (group) {
      comparison=NULL
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
      names(groups)[1]<-name.y
      if(console) {
        cat("\nTreatments with the same letter are not significantly different.\n\n")
        print(groups)  
      }   
    }
    parameters<-data.frame(test="Fisher-LSD",p.ajusted=p.adj,name.t=name.t,ntr = ntr,alpha=alpha)
    rownames(parameters)<-" "
    rownames(statistics)<-" "
    rownames(means)<-means[,1]
    means<-means[,-1]
    output<-list(statistics=statistics,parameters=parameters, 
                 means=means,comparison=comparison,groups=groups)
    class(output)<-"group"
    invisible(output)
  }
