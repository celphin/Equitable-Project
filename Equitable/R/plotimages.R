plotimages <-
function(orig,E,ls,row_unit=NULL,col_unit=NULL,
                     signal=NULL,Eave, genname=NULL,zlimits=NULL, of=FALSE,lf=FALSE,ef=TRUE,yline=3,yma=5,
                     avef=FALSE,fcontour=TRUE){

if(is.null(zlimits)) zlimits<-c(min(E,na.rm=TRUE),max(E,na.rm=TRUE))
if(!is.infinite(zlimits[1])){
if(zlimits[2]<= zlimits[1]+1e-6)zlimits<-c(zlimits[1]-1/10,zlimits[1]+1/10)   #;cbflag<-FALSE} else cbflag<-TRUE


if(is.null(genname))genname<-"Data"
if(fcontour) {
if(of && sd(orig,na.rm=TRUE)>1e-6)contour(orig,main=paste0("Original",genname), zlim=zlimits/1,xlab=row_unit,ylab=col_unit)
if(!is.null(signal) && sd(signal,na.rm=TRUE)>1e-6)contour(signal,main=paste0("No Noise Signal: ",genname), zlim=zlimits/1,xlab=row_unit,ylab=col_unit)

if(ef && sd(E,na.rm=TRUE)>1e-6 )contour(E,main=paste0("T Noise Equitable",genname), zlim=zlimits/1,xlab=row_unit,ylab=col_unit)
if(avef && sd(Eave,na.rm=TRUE)>1e-6)contour(Eave,main=paste0("T Noise Equitable REFERENCED ",genname), zlim=zlimits/1,xlab=row_unit,ylab=col_unit)
if(lf && sd(ls,na.rm=TRUE)>1e-6)contour(ls,main=paste0("T Noise Least squared: ",genname), zlim=zlimits/1,xlab=row_unit,ylab=col_unit)
}
if(of)imagenan(orig,main=paste0("Original: ",genname), zlim=zlimits/1,row_unit=row_unit,col_unit=col_unit,yma=yma,yline=yline)
if(!is.null(signal))imagenan(signal,main=paste0("No Noise Signal: ",genname), zlim=zlimits/1,row_unit=row_unit,col_unit=col_unit)

if(ef)imagenan(E,main=paste0("T Noise Equitable : ",genname), zlim=zlimits/1,row_unit=row_unit,col_unit=col_unit,yma=yma,yline=yline)
if(avef)imagenan(Eave,main=paste0("T Noise Equitable REFERENCED : ",genname), zlim=zlimits/1,row_unit=row_unit,col_unit=col_unit,yma=yma,yline=yline)
if(lf)imagenan(ls,main=paste0("T Noise Least squared: ",genname), zlim=zlimits/1,row_unit=row_unit,col_unit=col_unit,yma=yma,yline=yline)
} else cat("\n ERROR: images transform is all nan")
}
