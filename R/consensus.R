#' consensus of clusters
#' 
#' The criterion of the consensus is to produce many trees by means of boostrap
#' and to such calculate the relative frequency with members of the clusters.
#' 
#' distance: "euclidean", "maximum", "manhattan", "canberra", "binary",
#' "minkowski", "gower", "chisq".  Method: "ward", "single", "complete",
#' "average", "mcquitty", "median", "centroid".  see functions: dist(),
#' hclust() and daisy() of cluster.
#' 
#' @param data data frame
#' @param distance method distance, see dist()
#' @param method method cluster, see hclust()
#' @param nboot The number of bootstrap samples desired.
#' @param duplicate control is TRUE other case is FALSE
#' @param cex.text size text on percentage consensus
#' @param col.text color text on percentage consensus
#' @param \dots parameters of the plot dendrogram
#' @return \item{table.dend }{The groups and consensus percentage}
#' \item{dendrogram}{The class object is hclust, dendrogram plot}
#' \item{duplicate}{Homonymous elements}
#' @author F. de Mendiburu
#' @seealso \code{\link{hclust}}, \code{\link{hgroups}}, \code{\link{hcut}}
#' @references An Introduction to the Boostrap. Bradley Efron and Robert J.
#' Tibshirani. 1993. Chapman and Hall/CRC
#' @keywords cluster
#' 
#' @importFrom stats as.dist dist  cutree hclust
#' @importFrom cluster daisy
#' @importFrom graphics plot text
#' @export
#' @examples
#' 
#' library(agricolae)
#' data(pamCIP)
#' # only code
#' rownames(pamCIP)<-substr(rownames(pamCIP),1,6)
#' output<-consensus( pamCIP,distance="binary", method="complete",nboot=5)
#' # Order consensus
#' Groups<-output$table.dend[,c(6,5)]
#' Groups<-Groups[order(Groups[,2],decreasing=TRUE),]
#' print(Groups)
#' ## Identification of the codes with the numbers.
#' cbind(output$dendrogram$labels)
#' ## To reproduce dendrogram
#' dend<-output$dendrogram
#' data<-output$table.dend
#' plot(dend)
#' text(data[,3],data[,4],data[,5])
#' # Other examples
#' # classical dendrogram
#' dend<-as.dendrogram(output$dendrogram)
#' plot(dend,type="r",edgePar = list(lty=1:2, col=2:1))
#' text(data[,3],data[,4],data[,5],col="blue",cex=1)
#' plot(dend,type="t",edgePar = list(lty=1:2, col=2:1))
#' text(data[,3],data[,4],data[,5],col="blue",cex=1)
#' ## Without the control of duplicates
#' output<-consensus( pamCIP,duplicate=FALSE,nboot=5)
#' ## using distance gower, require cluster package.
#' # output<-consensus( pamCIP,distance="gower", method="complete",nboot=5)
#' 
consensus <-
  function(data,distance=c("binary","euclidean","maximum","manhattan","canberra",
                           "minkowski","gower","chisq"),method=c("complete","ward","single","average","mcquitty","median",
                                                         "centroid"),nboot=500,duplicate=TRUE,cex.text=1,col.text="red", ...)
  {
    t0<-Sys.time()
    distance <- match.arg(distance)
    method <- match.arg(method)
    if(distance=="gower") {
      if (requireNamespace("cluster", quietly = TRUE)) {
        distancia<-cluster::daisy(data,metric=distance)
      }
      else{
        return("Please install cluster package for calculate gower distance")
      }
    }
    if(distance=="chisq") {
      n<-sum(data)
      nr<-nrow(data);nc<-ncol(data)
      ro<-apply(data,1,function(x)sum(x,na.rm=TRUE))
      co<-apply(data,2,function(x)sum(x,na.rm=TRUE))/n
      B<-data/ro
      A<-matrix(0,nr,nr)
      colnames(A)<-rownames(A)<-rownames(data)
      for(i in 1:nr){
        for(j in 1:nr){d<-0
        for(k in 1:nc) d<-d+(B[i,k]-B[j,k])^2/co[k]
        A[i,j]<-sqrt(d)
        }}
      distancia<-as.dist(A)
    }
    if(distance!="chisq" & distance!="gower")distancia<-dist(data,method=distance)
    nc<-ncol(data)
    nr<-nrow(data)
    dend<-hclust(distancia,method=method)
    h1<-cutree(dend,h=0)
    h2<-data.frame(h1)
    h3<-unique(h2)
    dup<-duplicate
    duplicate<-NULL
    # To study duplicate
    if(dup){
      if(nrow(h3) < length(h1)){
        nr<-nrow(h3)
        data<-data.frame(d=rownames(data),data)
        h3<-data.frame(d=rownames(h3),h3)
        duplicate<- merge(h3,data,by="d",all=TRUE)
        duplicate<-duplicate[is.na(duplicate[,2]),]
        dup0<-duplicate[,-2]
        duplicate<-as.character(duplicate$d)
        data<-merge(h3,data,by="d")
        dup1 <-data[,-2]
        dup0<-cbind(dup0,unique="")
        dup0[,1]<-as.character(dup0[,1])
        dup1[,1]<-as.character(dup1[,1])
        ncdup<-ncol(dup1)
        dup0[,ncdup+1]<-as.character(dup0[,ncdup+1])
        ndup0<-nrow(dup0)
        ndup1<-nrow(dup1)
        ncdup<-ncol(dup1)
        for ( i in 1:ndup0) {
          for ( j in 1:ndup1) {
            if(sum(dup0[i,2:ncdup]==dup1[j,-1],na.rm=TRUE) == ncdup-1){
              dup0[i,ncdup+1]<-dup1[j,1]
              break
            }
          }
        }
        if (sum(dup0[,ncdup+1]=="")>0) {
          add1<-dup0[dup0[,ncdup+1]=="",]
          add1<-data.frame(d=add1[,1],h1=0,add1[,2:ncdup])
          data<-rbind(data,add1)
        }
        rownames(data)<-data[,1]
        data<-data[,c(-1,-2)]
        nc<-ncol(data)
        if(distance=="gower") distancia<-daisy(data,metric=distance)
        if(distance=="chisq") {
          n<-sum(data)
          nr<-nrow(data);nc<-ncol(data)
          ro<-apply(data,1,function(x)sum(x,na.rm=TRUE))
          co<-apply(data,2,function(x)sum(x,na.rm=TRUE))/n
          B<-data/ro
          A<-matrix(0,nr,nr)
          colnames(A)<-rownames(A)<-rownames(data)
          for(i in 1:nr){
            for(j in 1:nr){d<-0
            for(k in 1:nc) d<-d+(B[i,k]-B[j,k])^2/co[k]
            A[i,j]<-sqrt(d)
            }}
          distancia<-as.dist(A)
        }
        if(distance!="chisq" & distance!="gower")distancia<-dist(data,method=distance)
        dend<-hclust(distancia,method=method)
        duplicate<-dup0[dup0[,ncdup+1]!="",][,c(1,ncdup+1)]
        names(duplicate)[1]<-"duplicate"
      }}
    nr<-nrow(data)
    if(!is.null(duplicate)) {
      cat("\nDuplicates:",nrow(duplicate))
      cat("\nNew data  :", nr,"Records\n")
    }
    
    dinicio<-dend$merge
    d0<-hgroups(dinicio)
    clases<- data.frame(d=d0,height=dend$height,sq=1:length(d0))
    #rownames(clases)<-d0
    
    b<- nboot
    d<-NULL
    #########################
    for ( k in 1: b) {
      muestra<-sample(1:nc,replace=TRUE)
      boot1<-data[,muestra]
      if(distance=="gower") distancia<-daisy(boot1,metric=distance)
      if(distance=="chisq") {
          n<-sum(boot1)
          nr<-nrow(boot1);nc<-ncol(boot1)
          ro<-apply(boot1,1,function(x)sum(x,na.rm=TRUE))
          co<-apply(boot1,2,function(x)sum(x,na.rm=TRUE))/n
          B<-data/ro
          A<-matrix(0,nr,nr)
          colnames(A)<-rownames(A)<-rownames(data)
          for(i in 1:nr){
            for(j in 1:nr){d<-0
            for(k in 1:nc) d<-d+(B[i,k]-B[j,k])^2/co[k]
            A[i,j]<-sqrt(d)
            }}
          distancia<-as.dist(A)
        }
        if(distance!="chisq" & distance!="gower")distancia<-dist(boot1,method=distance)    
      d1<-hclust(distancia,method=method)$merge
      d1<-hgroups(d1)
      d<-rbind(d,d1)
    }
    td<-table(d)
    td<-data.frame(td)
    junto<-merge(clases,td,by="d")
    junto[,4]<-junto[,4]*100/b
    junto<-merge(clases,junto,by="d",all=TRUE)
    junto[is.na(junto)]<-0
    junto<-junto[order(junto[,3]),]
    ############
    tiempo<- Sys.time()-t0
    unidad <- attributes(tiempo)$units
    cat("\nConsensus hclust\n" )
    cat("\nMethod distance:",distance)
    cat("\nMethod cluster :",method)
    cat("\nrows and cols  :",nr,nc)
    cat("\nn-bootstrap    :",nboot)
    cat("\nRun time       :",tiempo,unidad,"\n\n")
    cc<-cbind(dend$merge,height=dend$height,percentage=round(junto[,6],1))
    co<-dend$order
    n1<-nrow(cc)
    n2<-length(co)
    p<-rep(0,n1)
    for(i in 1:n1) {
      if ((cc[i,1] < 0) & (cc[i,2] < 0) ) {
        for(k in 1:n2) {
          if(co[k]==-cc[i,1]) k1=k
          if(co[k]==-cc[i,2]) k2=k
        }
        p[i]<- (k1+k2)/2
      }
      if ((cc[i,1]) < 0 & (cc[i,2] > 0)) {
        for(k in 1:n2) {
          if(co[k]==-cc[i,1]) k1=k
        }
        p[i]<-(k1+p[cc[i,2]])/2
      }
      if ((cc[i,1] > 0) & (cc[i,2] < 0)) {
        for(k in 1:n2) {
          if(co[k]==-cc[i,2]) k1=k
        }
        p[i]<-(k1+p[cc[i,1]])/2
      }
      if ((cc[i,1] > 0) & (cc[i,2] > 0)) {
        p[i]<-(p[cc[i,1]]+p[cc[i,2]])/2
      }
    }
    table.dend <- data.frame(dend$merge,xaxis=p,cc[,3:4],groups=d0)
    plot(dend,...)
    #delta<-0.05*(max(cc[,4]))
    text(p,cc[,3],ceiling(cc[,4]),cex=cex.text,col=col.text)
    return(list(table.dend=table.dend,dendrogram=dend,duplicates=duplicate) )
  }
