make_A <-
function(xref,f){
  A<- matrix(NA,nrow=length(f), ncol=length(f))
  A[,xref]= f
  A[abs(A[,xref])<1e-10]<-1e-10
  A<-sapply(1:ncol(A),function(c){A[,c]<-A[,xref]/A[c,xref]})  # plot(A[,xref])
  #imagenan(A,main="slope matrix from f",zlim=c(-2,2)) #A[1,1]
  sdA<-sd(A,na.rm=TRUE)
  mA<-mean(A,na.rm=TRUE)
  zlim<-c(mA-0.5*sdA,mA+0.5*sdA)
 # imagenan(A,main="slope matrix from f",zlim=zlim)
  return(A)
}
