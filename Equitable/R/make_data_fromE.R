make_data_fromE <-
function(orig,A,B=NULL,zero=0,maxA=3, main=" "){
  orig<-as.matrix(orig)
 # if(is.null(B)){B<-matrix(0,nrow=nrow(A),ncol=ncol(A))}
   AI<-AI_Bmult_NA(A=A,I=orig,B=B,maxA=maxA)
  #imagenan(AI)#A<-Td1$E.s; B<-Td1$E.b;orig<-Td1$smat  ;orig[5:10,1:3]<-NA   ;imagenan(orig)
    Tx<-AI$xme  #attributes(AI) imagenan(Tx) ;imagenan(Tsd);imagenan(TN)
    Tsd<-AI$xsd
    TN<-AI$N               #imagenan(orig) ;imagenan(Tx) ;imagenan(Tsd);imagenan(TN)
    sdo<-sd(orig,na.rm=TRUE)
    mo<-mean(orig,na.rm=TRUE)
    zlim<-c(mo-1.5*sdo,mo+1.5*sdo)
    imagenan(Tx,main=paste(main,"Equitable"),zlim=zlim);
    imagenan(orig,main=paste(main,"Original"),zlim=zlim); cat("\nresidual std (Simple Transform-Orig)",main, sd(Tx-orig))
    imagenan(orig-Tx,main=paste(main,"Residuals"),zlim=zlim)
    plot(Tx,orig,main=paste(main," vs Original"))  ;lines(orig,orig)
    Tsimple<-list(Tx=Tx,Tsd=Tsd,TN=TN)
    return(Tsimple)

}
