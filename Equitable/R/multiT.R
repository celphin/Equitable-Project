multiT <-
function(Td_noise,numrun=1,maxrun=5,minme=0.001, minstd=0.015){

  if(numrun==1){imagenan(Td_noise$smat,main="Original"); imagenan(Td_noise$ET.x,main="T[1]=T(I)")}
  cat("\n\nmultiTI: run ",numrun)

  if(numrun>maxrun){
    cat("\n\n\nNo Convergence: ending at numrun=",numrun)
    if(is.null(Td2)) Td2<-Td_noise
    return(Td2)
  }
  Td2<-transformE(Td_noise$ET.x, Ave=FALSE,diagonal=FALSE,old=Td_noise)

  me<-abs(mean((Td2$ET.x-Td_noise$ET.x)/Td_noise$ET.x,na.rm=TRUE))
  std<-sd((Td2$ET.x-Td_noise$ET.x)/Td_noise$ET.x,na.rm=TRUE)
  cat("\n\nmean of relative error",me)
  cat("\nsd of relative error",std)
  imagenan(Td2$ET.x,main=paste0("T(",numrun+1,")=T(T[",numrun,"])"))

  #stats s
  numrun<-numrun+1
  if(me< minme && std<minstd){
    cat("\n\n\nCONVERGENCE REACHED: T[n]=T[n-1] or T[n-1]=T(T[n-1])at numrun=",numrun)
    return(Td2)
  }

  multiT(Td_noise=Td2,numrun=numrun)

}
