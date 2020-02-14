kruskal <-
  function (y, trt, alpha = 0.05, p.adj = c("none", "holm", "hommel", "hochberg", 
    "bonferroni", "BH", "BY", "fdr"), group = TRUE, main = NULL,console=FALSE) 
  {
    name.y <- paste(deparse(substitute(y)))
    name.t <- paste(deparse(substitute(trt)))
    if(is.null(main))main<-paste(name.y,"~", name.t)
    p.adj <- match.arg(p.adj)
    junto <- subset(data.frame(y, trt), is.na(y) == FALSE)
    N <- nrow(junto)
    medians<-tapply.stat(junto[,1],junto[,2],stat="median")
    for(i in c(1,5,2:4)) {
      x <- tapply.stat(junto[,1],junto[,2],function(x)quantile(x)[i])
      medians<-cbind(medians,x[,2])
    }
    medians<-medians[,3:7]
    names(medians)<-c("Min","Max","Q25","Q50","Q75")
    Means <- tapply.stat(junto[,1],junto[,2],stat="mean")  #change
    sds <-   tapply.stat(junto[,1],junto[,2], stat="sd")  #change
    nn <-   tapply.stat(junto[,1],junto[,2],stat="length") #change
    Means<-data.frame(Means,std=sds[,2],r=nn[,2],medians)    
    rownames(Means)<-Means[,1]
    Means<-Means[,-1]
    names(Means)[1]<-name.y   
    junto[, 1] <- rank(junto[, 1])
    means <- tapply.stat(junto[, 1], junto[, 2], stat = "sum")
    sds <- tapply.stat(junto[, 1], junto[, 2], stat = "sd")
    nn <- tapply.stat(junto[, 1], junto[, 2], stat = "length")
    means <- data.frame(means, r = nn[, 2])
    names(means)[1:2] <- c(name.t, name.y)
    ntr <- nrow(means)
    nk <- choose(ntr, 2)
    DFerror <- N - ntr
    rs <- 0
    U <- 0
    for (i in 1:ntr) {
      rs <- rs + means[i, 2]^2/means[i, 3]
      U <- U + 1/means[i, 3]
    }
    S <- (sum(junto[, 1]^2) - (N * (N + 1)^2)/4)/(N - 1)
    H <- (rs - (N * (N + 1)^2)/4)/S
    p.chisq <- 1 - pchisq(H, ntr - 1)
    if(console){
      cat("\nStudy:", main)
      cat("\nKruskal-Wallis test's\nTies or no Ties\n")
      cat("\nCritical Value:", H)
      cat("\nDegrees of freedom:", ntr - 1)
      cat("\nPvalue Chisq  :", p.chisq, "\n\n")
    }
    DFerror <- N - ntr
    Tprob <- qt(1 - alpha/2, DFerror)
    MSerror <- S * ((N - 1 - H)/(N - ntr))
    means[, 2] <- means[, 2]/means[, 3]
    if(console){cat(paste(name.t, ",", sep = ""), " means of the ranks\n\n")
      print(data.frame(row.names = means[, 1], means[, -1]))
      cat("\nPost Hoc Analysis\n")       
    }
    if (p.adj != "none") {
      if(console)cat("\nP value adjustment method:", p.adj)
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
    nr <- unique(means[, 3])
    
    if (group & console){
      cat("\nt-Student:", Tprob)
      cat("\nAlpha    :", alpha)}

    if (length(nr) == 1)  LSD <- Tprob * sqrt(2 * MSerror/nr)
    statistics<-data.frame(Chisq=H,Df=ntr-1,p.chisq=p.chisq)
    if ( group & length(nr) == 1 & console) cat("\nMinimum Significant Difference:",LSD,"\n")
    if ( group & length(nr) != 1 & console) cat("\nGroups according to probability of treatment differences and alpha level.\n")
    if ( length(nr) == 1) statistics<-data.frame(statistics,t.value=Tprob,MSD=LSD)
    
    comb <- utils::combn(ntr, 2)
    nn <- ncol(comb)
    dif <- rep(0, nn)
    LCL <- dif
    UCL <- dif
    pvalue <- dif
    sdtdif <- dif
    for (k in 1:nn) {
      i <- comb[1, k]
      j <- comb[2, k]
      dif[k] <- means[i, 2] - means[j, 2]
      # S * ((N - 1 - H)/(N - ntr))
      sdtdif[k] <- sqrt(MSerror * (1/means[i,3] + 1/means[j, 3]))
      pvalue[k] <- 2*(1 - pt(abs(dif[k])/sdtdif[k],DFerror))
    }
    if (p.adj != "none") pvalue <- p.adjust(pvalue, p.adj)
    pvalue <- round(pvalue,4)
    sig <- rep(" ", nn)
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
    comparison <- data.frame(Difference = dif, pvalue = pvalue, "Signif."=sig, LCL, UCL)
    if (p.adj !="bonferroni" & p.adj !="none"){
      comparison<-comparison[,1:3]
      statistics<-data.frame(Chisq=H,p.chisq=p.chisq)
    }
    rownames(comparison) <- paste(tr.i, tr.j, sep = " - ")
    if (!group) {
      groups<-NULL
      if(console){
        cat("\nComparison between treatments mean of the ranks.\n\n")
        print(comparison)
      }
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
      groups <- orderPvalue(means[, 1], means[, 2],alpha, Q, console)
      names(groups)[1]<-name.y
      if(console) {
      cat("\nTreatments with the same letter are not significantly different.\n\n")
      print(groups)
      } 
    }
    ranks=means
    Means<-data.frame(rank=ranks[,2],Means)
    Means<-Means[,c(2,1,3:9)]
    parameters<-data.frame(test="Kruskal-Wallis",p.ajusted=p.adj,name.t=name.t,ntr = ntr,alpha=alpha)
    rownames(parameters)<-" "
    rownames(statistics)<-" "
    output<-list(statistics=statistics,parameters=parameters, 
                 means=Means,comparison=comparison,groups=groups)    
    class(output)<-"group"
    invisible(output)
  }
