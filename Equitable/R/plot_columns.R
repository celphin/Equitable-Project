plot_columns <-
function(d,x_unit="INDEX",y_unit="DATA VALUE",
                       main="Data plots", limits=NULL,
                       xlimits=NULL){
  #min(d,na.rm=TRUE)
  if(is.null(limits)) limits<-c(min(d,na.rm=TRUE),max(d,na.rm=TRUE))
  if(is.null(xlimits)) xlimits<-c(1,nrow(d))
  #par(fig=c(0.,0.8,0.,0.8), new=FALSE)
  par(mfrow=c(1,1))
  plot(d[,1],ylim=limits,xlim=xlimits, main=main,xlab=x_unit, ylab=y_unit,xaxt='n',yaxt='n')
  if(is.null(y_unit)) y_unit<-"Data Value"
  if(is.null(x_unit)) x_unit<-"Index"
  rtick<-1
  er<-nrow(d)
  rtickinc<-(er-rtick)/10
  rnames<-rownames(d)
  axis(1,at=c(seq(rtick,er,by= rtickinc)),labels=rnames[seq(rtick,er,by=rtickinc)],lwd=2)
  axis(2,ylim= limits,lwd=2)
  #legend("topright", legend = paste0("Slopes at Reference ", cnames[j]))
  apply(as.data.frame(seq(ncol(d),1, by=(-1))),1, FUN=function(v){ lines(d[,v],type="o", pch=v%%25,col=v) })
  return(limits)
}
