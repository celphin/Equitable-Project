plot_columnsnum <-
function(d,
                          main="Data plots", xlab="Index",limits=NULL,
                          xlimits=NULL){
  #min(d,na.rm=TRUE)
  if(is.null(limits)) limits<-c(min(d,na.rm=TRUE),max(d,na.rm=TRUE))
  if(is.null(xlimits)) xlimits<-c(1,ncol(d))
  plot(d[1,],ylim=limits,xlim=xlimits, main=main, ylab="Data Value",xlab=xlab)

  apply(as.data.frame(seq(2,nrow(d), by=1)),1, FUN=function(v){ lines(d[v,],type="o", pch=paste0(v%%25),col=v) })  # paste0(v%%10)
  return(limits)
}
