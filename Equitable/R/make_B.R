make_B <-
function(xref,orig,A){
  B<- matrix(NA,nrow=nrow(A), ncol=ncol(A))
  cme<-colMeans(orig,na.rm=TRUE)  ;
  plot(cme,pch=15,type="b",main="Time averaged column variation")  #imagenan(cm)
  #plot(cm)
  # Itave<-t(matrix(rep(cm,nrow(orig)),nrow=ncol(orig),ncol=nrow(orig)))  ;imagenan(mcm)
  # newd<-orig-Itave;
  B[,xref]<- cme-A[,xref]*cme[xref]  #plot(B[,xref])    plot(A[,xref]) plot(cme)
  B<-sapply(1:ncol(B),function(c){B[,c]<-B[,xref]-A[,c]*B[c,xref]})
  sdB<-sd(B,na.rm=TRUE)
  mB<-mean(B,na.rm=TRUE)
  zlim<-c(mB-1.*sdB,mB+1.*sdB)   #plot(B[,xref])
  #imagenan(B,main="intercept matrix from Time Average and slope",zlim=zlim)
  return(B)
}
