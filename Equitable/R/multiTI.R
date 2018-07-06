multiTI <-
function(Td_noise,numrun=1,maxrun=5,minme=0.001, minstd=0.015){

  if(numrun==1){imagenan(Td_noise$smat,main="Original"); imagenan(Td_noise$ET.x,main="T[1]=T(I)")}
  cat("\n\nmultiTI: run ",numrun)

  if(numrun>maxrun){
    cat("\n\n\nNo Convergence: ending at numrun=",numrun)
    if(!is.null(Td2)){
      Td2$Ave.ET.x<-ET2$Ave.ET.x
      Td2$Ave.ET.xsd<-ET2$Ave.ET.xsd
      Td2$Ave.ET.Es<-ET2$Ave.ET.Es
      Td2$Ave.ET.Eb<-ET2$Ave.ET.Eb
      Td2$Ave.ET.Ep <-ET2$Ave.ET.Ep
    } else Td2<-Td_noise
    return(Td_noise)
  }
  ET2<-transave1(Td_noise$ET.x,Td_noise,x=1:ncol(Td_noise$ET.x),equita=TRUE,diagonal=TRUE)
  #summary(ET1)
  me<-abs(mean((ET2$Ave.ET.x-Td_noise$ET.x)/Td_noise$ET.x,na.rm=TRUE))
  std<-sd((ET2$Ave.ET.x-Td_noise$ET.x)/Td_noise$ET.x,na.rm=TRUE)
  cat("\n\nmean of relative error",me)
  cat("\nsd of relative error",std)
  imagenan(ET2$Ave.ET.x,main=paste0("T(",numrun+1,")=T(T[",numrun,"])"))
  Td2<-Td_noise
  Td2$ET.x<-ET2$Ave.ET.x
  Td2$ET.xsd<-ET2$Ave.ET.xsd
  Td2$ET.Es<-ET2$Ave.ET.Es
  Td2$ET.Eb<-ET2$Ave.ET.Eb
  Td2$ET.Ep<-ET2$Ave.ET.Ep


  #stats s
  numrun<-numrun+1
  if(me< minme && std<minstd){
    cat("\n\n\nCONVERGENCE REACHED: T[n]=T[n-1] or T[n-1]=T(T[n-1])at numrun=",numrun)
    Td2$Ave.ET.x<-ET2$Ave.ET.x
    Td2$Ave.ET.xsd<-ET2$Ave.ET.xsd
    Td2$Ave.ET.Es<-ET2$Ave.ET.Es
    Td2$Ave.ET.Eb<-ET2$Ave.ET.Eb
    Td2$Ave.ET.Ep <-ET2$Ave.ET.Ep
    return(Td2)
  }

  multiTI(Td_noise=Td2,numrun=numrun)

}
