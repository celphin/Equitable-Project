plot_vsref <-
function(d,ref,
                     main="Data plots", xlab="Data Value (Reference)",limits=NULL,
                     lty="p",legf=FALSE,cex.main=0.8){
  if(length(d[,ref])<18 && legf) colourevent<-c((1:length(d[,ref]))+9) else colourevent<-"black"
  ref<-as.numeric(ref)
  #min(d,na.rm=TRUE)
  if(is.null(limits)){
   #  xlimits<-c(min(d[,ref],na.rm=TRUE),max(d[,ref],na.rm=TRUE))
   # ylimits<-c(min(d,na.rm=TRUE),max(d,na.rm=TRUE))
   mins<-min(d,na.rm = TRUE)
   maxs<-max(d,na.rm = TRUE)
   ds<-maxs-mins
   xlimits<-ylimits<-c(mins-1*ds*3/8,maxs+ds*3/8)
  }
  else{
    ylimits<-xlimits<-limits
  }
  plot(d[,ref],d[,ref],ylim=ylimits,xlim=xlimits , ylab="Data Value",xlab=xlab, lwd=3,cex=1.5,col=colourevent)
  title(main=main,cex.main=cex.main)
  apply(as.data.frame(seq(1,ncol(d), by=1)),1, FUN=function(v,d,ref){ lines(d[,ref],d[,v],type=lty, pch=v%%25,col=v) },ref,d=as.data.frame(d))

  if(legf)legend("bottomright",inset= 0.0,(rownames(d)), fill=colourevent )
}
