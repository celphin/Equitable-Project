make_datafrom_eigen <-
function(eigenvect,orig,xref=1,sgn=1,main=" "){
  orig<-as.matrix(orig)
  #   #make equitabe matrix from this eigenvector
f<-eigenvect

#plot(f);lines(eigenvect,type="b",pch="O")
cm<-colMeans(orig,na.rm=TRUE)  #;plot(cm)
mcm<-t(matrix(rep(cm,nrow(orig)),nrow=ncol(orig),ncol=nrow(orig)))  #;imagenan(mcm)
newd<-orig-mcm;#imagenan(newd); plot(f)

sdc<-sapply(1:ncol(newd),function(c){sd(newd[,c],na.rm=TRUE)})
F<-matrix(rep(f,length(f)),nrow=length(f),ncol=length(f))   #diag(F)
F<-sapply(1:ncol(F),function(c){F[,c]<-F[,c]/F[c,c]})   #imagenan(F)  imagenan(Td) plot(F[,1])  plot(f)
testd<- F%*%t(newd)/ncol(F)   # imagenan(t(testd));  imagenan(t(newd))  imagenan(t(testd)-newd)
sdc<-sapply(1:ncol(newd),function(c){
 sd(newd[,c]-t(testd)[,c],na.rm=TRUE)/sd(newd[,c],na.rm=TRUE)
  })
F<-matrix(rep(f,length(f)),nrow=length(f),ncol=length(f))   #diag(F)
F<-sapply(1:ncol(F),function(c){F[,c]<-F[,c]/F[c,c]})   #imagenan(F)  imagenan(Td) plot(F[,1])  plot(f)
testd<- -F%*%t(newd)/ncol(F)   # imagenan(t(testd));  im
sdc1<-sapply(1:ncol(newd),function(c){
  sd(newd[,c]-t(testd)[,c],na.rm=TRUE)/sd(newd[,c],na.rm=TRUE)
})
# plot(sdc,main="sign is 1"); plot(sdc1,main="sign is -1")
if(min(sdc)<min(sdc1)){
  sgn<-1
  xref<-which(sdc==min(sdc))
  } else {
    sgn<- (-1)
    xref<-which(sdc1==min(sdc1))

  }
cat("\nsign ",sgn," ref ",xref,"\n")
#   #imagenan(orig); imagenan(newd)
# f<- sgn*f     #eigenvectors.t %*% newdata.t   imagenan(newd)  xref<-3
# xref<-ncol(orig);
cat("\nSIGN ",sgn)
A<-make_A(xref,f)   #imagenan(A);imagenan(B)

# Tdata<-make_data_fromE(orig=newd,A=A,zero=0, main=main,maxA=50)
# Tx<-Tdata$Tx+mcm   #imagenan(Tdata$Tx)
# imagenan(Tx,main="Data From Eigenvector+mcm"); cat("\nResid errorTransformEigen-orig ",sd(Tx-(orig),na.rm=TRUE),"\n") #

B<-make_B(xref=xref,orig=sgn*orig,A=A) #plot(B[,ncol(B)]) ; plot(A[,ncol(A)])   : imagenan(orig)

Tdata<-make_data_fromE(orig=(orig),A=(sgn*A),B=B,zero=0, main=main,maxA=50)
Tx<-Tdata$Tx
#imagenan(Tx,main="Data From Eigenvector");# cat("\nResid error Transform(Eigen)-orig ",sd(Tx-(orig)),"\n") #

# ones<-matrix(rep(1,ncol(orig)*nrow(orig)),nrow=ncol(A),ncol=nrow(orig))
# #Tdata<- A[:1:13] %*%t(orig)[1:13,] /ncol(A)+ B%*%ones/ncol(B)  # plot(sapply(1:nrow(newd),function(t)sum(A[15,1:10]*t(newd)[1:10,t])))
# Tdata<- sgn*A[,1:13] %*%t(newd)[1:13,] /ncol(A)+ B%*%ones/ncol(B)    #mean(B%*%ones/ncol(B))   imagenan(sgn*A %*%t(newd) /ncol(A))   plot(A[3,])
# #imagenan(matrix(Tdata,ncol=ncol(A),nrow=nrow(newd)))
# #plot(A[1:21,]%*%t(newd)[,200])     # ncol(t(newd))   plot(t(newd)[20,]) ncol(t(newd))

                                       #imagenan(t(e1)) imagenan(t(e2),zlim=c(-0.1,0.1)) ; imagenan(Tdata,zlim=c(-0.1,0.1))
#imagenan((orig),main="Original")  # imagenan(orig) ; imagenan(orig,zlim=c(-0.1,0.1))
 return(Tdata)
}
