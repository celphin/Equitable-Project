plotversus <-
function(orig,E,ls,
                     signal=NULL,Eave,ylimits=NULL,genname=NULL, of=FALSE,lf=FALSE,ef=TRUE,
                     avef=FALSE) {
  if(is.null(genname))genname<-"Data"
  if(is.null(ylimits)) ylimits<-c(min(E,na.rm=TRUE),max(E,na.rm=TRUE))
  if(is.null(signal)){   #null signal then plot orig vs others
    if(of){

      if(lf){
        plot(ls,orig, main="Noise: Original vs Least Squares Transform ",xlab="Least Squares Transform",ylab="Original Data", ylim=ylimits,xlim= ylimits)
        lines(seq(ylimits[1],ylimits[2],by = (ylimits[2]-ylimits[1])/100 ),seq(ylimits[1],ylimits[2],by = (ylimits[2]-ylimits[1])/100 ))
      }
      if(ef){
        plot(E,orig,main="Noise: Original vs Equitable Transform ",xlab="Equitable Transform",ylab="Original Data",ylim=ylimits,xlim= ylimits)
        lines(seq(ylimits[1],ylimits[2],by = (ylimits[2]-ylimits[1])/100 ),seq(ylimits[1],ylimits[2],by = (ylimits[2]-ylimits[1])/100 ))
      }
      if(avef){
        plot(Eave,orig,main="Noise: Original vs Equitable Transform REFERENCED",xlab="Equitable Transform (Referenced)",ylab="Original Data",ylim=ylimits,xlim= ylimits)
        lines(seq(ylimits[1],ylimits[2],by = (ylimits[2]-ylimits[1])/100 ),seq(ylimits[1],ylimits[2],by = (ylimits[2]-ylimits[1])/100 ))
      }
    }
  } else {
      if(of){
      plot(signal,orig,main="Noise: Original Data vs Signal",xlab="Signal",ylab="Original Data",ylim=ylimits,xlim= ylimits)
lines(seq(ylimits[1],ylimits[2],by = (ylimits[2]-ylimits[1])/100 ),seq(ylimits[1],ylimits[2],by = (ylimits[2]-ylimits[1])/100 ))
}
if(lf){
  plot(signal,ls,main="Noise: Least Squares Transform vs Signal",xlab="Signal",ylab="Least Squares Transform",ylim=ylimits,xlim= ylimits)
lines(seq(ylimits[1],ylimits[2],by = (ylimits[2]-ylimits[1])/100 ),seq(ylimits[1],ylimits[2],by = (ylimits[2]-ylimits[1])/100 ))
}
if(ef){
  plot(signal,E,main="Noise: Equitable Transform vs Signal",xlab="Signal",ylab="Equitable Transform",ylim=ylimits,xlim= ylimits)
lines(seq(ylimits[1],ylimits[2],by = (ylimits[2]-ylimits[1])/100 ),seq(ylimits[1],ylimits[2],by = (ylimits[2]-ylimits[1])/100 ))
}
    if(avef){
      plot(signal,Eave,main="Noise: Equitable Transform REFERENCED vs Signal",xlab="Signal",ylab="Equitable Transform REFERENCED",ylim=ylimits,xlim= ylimits)
      lines(seq(ylimits[1],ylimits[2],by = (ylimits[2]-ylimits[1])/100 ),seq(ylimits[1],ylimits[2],by = (ylimits[2]-ylimits[1])/100 ))
    }
  }
}
