plotindivid <-
function(I,zw,lsz,
                      num=NULL,signal=NULL,Eave=NULL,xlimits=NULL,ylimits=NULL,genname=NULL, of=FALSE,lf=FALSE,ef=TRUE,avef=FALSE,err=FALSE,
                      Eerr=NULL, lserr=NULL,x_unit=NULL,y_unit=NULL,
                      Eaveerr=NULL ){

  #if(is.null(genname))genname<-"Data"
  if(is.null(ylimits)) ylimits<-c(min(zw,na.rm=TRUE)/1.0,max(zw,na.rm=TRUE)*1.0)
  if(is.null(xlimits)) xlimits<-c(1,nrow(zw))
  #cat("\nPlotindiv:xlimits: ", xlimits)
  #mu<-colMeans(I,na.rm = TRUE)
  if(is.null(num))num<-(ncol(I)-1)
  if(length(num)==1)listx<-seq(1,ncol(I), by=(ncol(I)-1)/num) else {
  if(length(num)==2 && num[1]==num[2])listx<-c(num[1]) else listx<-num
  }
  #cat("\nPlotindiv:Columns plotted ",listx," transpose=  :",transpose,"\n")
  #cat("\n",x_unit,y_unit)
  rnames<-rownames(I)
  for(y in listx){
    y<-floor(y)
    yn<-colnames(I)[y]
   if(ef){
     if(err){
       plotdata_with_errors(zw[,y],Eerr[,y],
                                  xlab=x_unit,ylab=paste(y_unit,yn),rnames=rnames,
                                  xlim=xlimits, main=paste("Index shown=",yn,"\nIncludes Error",genname),cex.main=0.7, ylim=ylimits, pch=11)
       } else {
                                    plot(zw[,y],xlab=x_unit,ylab=paste(y_unit,yn),
                          xlim=xlimits,ylim=ylimits, type="p",pch=11,main=paste("Index shown ",yn,"\n",genname),cex.main=0.7)

                                    }
    } else {
     if(of && !lf &&  !ef && !avef){
       plot(I[,y],xlab=x_unit,ylab=paste(y_unit,yn),main=paste("Index shown ",yn,"\n",genname),cex.main=0.7,
                            xlim=xlimits, ylim=ylimits, type="p",pch=15)
     } else {
      if(lf){
        plotdata_with_errors(lsz[,y],lserr[,y],xlab=x_unit,ylab=paste(y_unit,yn),rnames=rnames,xlim=xlimits,
                            main=paste("Index shown ",yn,"\n",genname),cex.main=0.7 ,ylim=ylimits, pch=1)
      } else {
        plotdata_with_errors(Eave[,y],Eaveerr[,y],xlab=x_unit,ylab=paste(y_unit,yn),rnames=rnames,
                               main=paste("Index shown ",yn,"\n",genname),cex.main=0.7 , xlim=xlimits,   ylim=ylimits, pch=0)
        }
      }
    }
   # plot(x[1,],pch=0)
    pch<-t((c(NA,NA,NA,NA,NA)))
    #pch<-t((c(15,24,11,0,NA)))
    #legend<-c('Original','Equitable','Least Squared','From Average','Signal')
    #lwd<-c(NA,NA,NA,NA,4)
    legend<-c(NA,NA,NA,NA,NA)
    lwd<-c(NA,NA,NA,NA,NA)
    if(!is.null(signal)){ legend[5]<-'Signal';lwd[5]<-4}
    if(of){legend[1]<-'Original'; pch[1]<-15}
    if(ef){legend[2]<-'Equitable'; pch[2]<-11}
    if(lf){legend[3]<-'Least Squared'; pch[3]<-1}
    if(avef){legend[4]<-'Eq. from Ave col'; pch[4]<-0}
    legend('topleft',inset=.02,legend=legend,
           lwd=lwd,pch = pch,bg='white',ncol=c(2),cex=0.75)

    if(of)lines(I[,y], type="p",pch=15);
    #lines(rep(mu[y],151), type="l",pch=11)
    if(!is.null(signal))lines(signal[,y], type="l",pch=15,lwd=4);
    if(lf)lines(lsz[,y], type="p",pch=1,lwd=2)
    if(avef)lines(Eave[,y], type="p",pch=0,lwd=2)

  }
}
