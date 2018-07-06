simpletran <-
function(I,mat) {      #mu are time row averages fro each column (space)
  s<-mat$Es;
  b<-mat$Eb;
  p<-mat$Ep

  zw<-matrix(NA,nrow=nrow(I),ncol=ncol(I))
  zsd<-matrix(NA,nrow=nrow(I),ncol=ncol(I))
  EN<-matrix(NA,nrow=nrow(I),ncol=ncol(I))
  for(y in 1:ncol(I)){
    for(t in 1:nrow(I)){
      zw[t,y]<-weighted.mean(s[y,]*I[t,]+b[y,],(1-p[y,])^2/sum((1-p[y,])^2, na.rm=TRUE) , na.rm=TRUE)
      zsd[t,y]<-sqrt(sum((1-p[y,])^2/sum((1-p[y,])^2, na.rm=TRUE) * (s[y,]*I[t,]+b[y,] - zw[t,y])^2,na.rm = TRUE ))
      EN[t,y]<-length(which(complete.cases(s[y,],b[y,])))
    }
  }

  cat("\n total of NA transformed data is ", sum(length(which(is.na(I)))))
  #image(zw)
  zw[which(is.na(zw))]<-I[which(is.na(zw))]   #copy dec 5 830am
  #image(zw)

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
