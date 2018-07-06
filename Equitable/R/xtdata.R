xtdata <-
function(I,x=NULL,t=NULL,imageplot=FALSE) {
  if(is.null(x)) x<-ncol(I)    #colnames(s)[nrow(s)]
  if(is.null(t)) t<-1:nrow(I)
  if(x[1]=="max"){
    x<-floor(which(abs(I)==max(abs(I),na.rm=TRUE))/nrow(I))
    cat("\nxtdata: x set to max row is ",x)
  }
  # cat("\nxtdata: x set to ",x)
  # cat("\nxtdata: t set to ",t)
  if(!is.null(x) && !is.null(t)){
    d<-matrix(NA,nrow=nrow(I),ncol=ncol(I))
    d[t,x]<-I[t,x]
    rownames(d)<-rownames(I)
    colnames(d)<-colnames(I)
    zlimits<-c(min(d,na.rm=TRUE),max(I,na.rm=TRUE)+1)
    if(imageplot)imagenan(d,main=paste0("Original "), zlim=zlimits/1)
  }
  return(d)
}
