AI_Bmult_NA <-
function(A,I,B=NULL,zero=0,maxA=NULL){
  I<-as.matrix(I-zero)
    #need to not run slope when larger than 3?
    if(!is.null(maxA)) A[A>maxA]<-NA
    if(is.null(B)){B<-matrix(0,nrow=nrow(A),ncol=ncol(A))}
    # ones<-Matrix(rep(1,ncol(I)*nrow(I)),nrow=nrow(I),ncol=ncol(I))
     #bval<-B1%*%ones/ncol(B1) # imagenan(bval);
    # Tsimple<-Tsimple1+bval + zero  #  imagenan(Td$E.b)

    AyI_By<-AyI_Byxme<-AyI_Byxsd<-AyI_ByN<-NULL
    for(t in 1:nrow(I)){
      It<-I[t,]
      xme<-xsd<-N<-NULL
    AIt_B<-sapply(1:ncol(A),function(y){
      xme<-mean((c(A[y,]*It+B[y,])),na.rm=TRUE)
      xsd<-sd(A[y,]*It+B[y,],na.rm=TRUE)
      N<-length(which(!is.na(A[y,]*It)))   #length(A[y,]) ; length(It)
      AIt_B<-list(xme=xme,xsd=xsd,N=N)
      return(AIt_B)
    })
    xme<-unlist(AIt_B["xme",]) #plot(xme)
    xsd<-unlist(AIt_B["xsd",])
    N<-unlist(AIt_B["N",])
    AyI_Byxme<-cbind(AyI_Byxme,xme)
    AyI_Byxsd<-cbind(AyI_Byxsd,xsd)
    AyI_ByN<-cbind(AyI_ByN,N)

    }
    rownames(AyI_Byxme)<-rownames(AyI_Byxsd)<-rownames(AyI_ByN)<-colnames(I)
    colnames(AyI_Byxme)<-colnames(AyI_Byxsd)<-colnames(AyI_ByN)<-rownames(I)
    AI_B<-list(xme=t(AyI_Byxme+zero),xsd=t(AyI_Byxsd),N=t(AyI_ByN)) #imagenan(AyI_Byxme)

    return(AI_B)
    # AI<-ABmult_NA(A=A,B=orig)  #imagenan(AI)
    # Tx<-AI$xme  #average,std dev and N  imagenan(Tx) ;imagenan(Tsd);imagenan(TN)
    # Tsd<-AI$xsd
    # TN<-AI$N
  }
