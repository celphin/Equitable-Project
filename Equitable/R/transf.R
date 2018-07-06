transf <-
function(I,mat,minp=0.5,equita=FALSE,diagonal=TRUE) {      #mu are time row averages fro each column (space)

    if(!equita){ s<-mat$s; b<-mat$b; node<-mat$node; se<-mat$sse }
    else {s<-mat$E.s; b<-mat$E.b; node<-mat$E.snode; se<-mat$E.sd1  #se<-mat$sse
    }

    ls<-mat$s
    lse<-mat$sse
    lb<-mat$b

    p<-mat$pslope

    # mu<-colMeans(I,na.rm = TRUE)

    #ls[p>minp]<-NA
    for (r in 1:nrow(ls))ls[abs(ls[r,])< abs(ls[,r]) & (abs(ls[r,])-lse[r,])< 0,r]<-NA
    s[is.na(ls)]=NA
    b[is.na(ls)]=NA

    frac<-sum(length(which(is.na(I))))/prod(dim(I))
    cat("\nFraction of data array that is missing is", frac)
    if(frac< 1/2)maxprob<- 2/3 else  maxprob<- 0.9
    p[is.nan(p)]<-NA
    if(length(p[!is.na(p)])>2){
    if(mean(1-p,na.rm=TRUE)==1)maxprob<- 1}
    cat("\nMaxprob set to : ",maxprob)
    for (r in 1:nrow(s))s[abs(s[r,])< abs(s[,r]) & (abs(s[r,])-lse[r,])< 0,r]<-NA
    b[is.na(s)]=NA

    for (r in 1:nrow(s)){
      crit=1
      qr<-quantile(abs(s[r,]),prob=maxprob,na.rm = TRUE)      #2/3 works welll Dec 10 740pm
      if(!is.na(qr) && qr>crit)crit=qr
      s[r,abs(s[r,])> abs(s[,r]) & abs(s[r,])> crit]<-NA   #crit is larger of 1 and quantile85
    }


    b[is.na(s)]=NA
    cat("\nDiagonal= ",diagonal)
    if(!diagonal){
      diag(s) <- NA
      diag(b) <- NA
    }


    zw<-matrix(NA,nrow=nrow(I),ncol=ncol(I))
    zsd<-matrix(NA,nrow=nrow(I),ncol=ncol(I))
    EN<-matrix(NA,nrow=nrow(I),ncol=ncol(I))
    for(y in 1:ncol(I)){
      for(t in 1:nrow(I)){

        zw[t,y]<-weighted.mean(s[y,]*I[t,]+b[y,],(1-p[y,])^2/sum((1-p[y,])^2, na.rm=TRUE) , na.rm=TRUE)
        zsd[t,y]<-sqrt(sum((1-p[y,])^2/sum((1-p[y,])^2, na.rm=TRUE) * (s[y,]*I[t,]+b[y,] - zw[t,y])^2,na.rm = TRUE ))
        EN[t,y]<-length(which(complete.cases(s[y,],b[y,])))
      }
      #zw[is.na(zw[,y]),y]<-mu[y]   #post dec 5 830AM
    }
    zw[is.nan(zw)]<-NA
    zsd[is.nan(zsd)]<-NA

   cat("\nequita=", equita," total of NA transformed data is ", sum(length(which(is.na(I)))))
   #image(zw)
   if(diagonal) zw[which(is.na(zw))]<-I[which(is.na(zw))]   #copy dec 5 830am
    #image(zw)
    colnames(zw)<-colnames(I) ; rownames(zw)<-rownames(I)
    colnames(zsd)<-colnames(I) ; rownames(zsd)<-rownames(I)
    colnames(EN)<-colnames(I) ; rownames(EN)<-rownames(I)

    Tx<-list(
      x=zw,
      xsd=zsd,
      EN=EN,
      Es=s,
      Eb=b,
      Ep=p
    )

    return(Tx)
  }
