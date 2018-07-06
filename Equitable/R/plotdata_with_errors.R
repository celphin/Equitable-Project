plotdata_with_errors <-
function( dataset,data_std,rnames=1:length(dataset),
                                 main="data",ylim=c(0,1),xlim=NULL,xlab="ROW",ylab="DATA VALUE",
                                 pch="O",type="p",col="black",lty=1,lineonly=FALSE,cex.main=1){

  #dataset and data_std are the data vector and error bars respectively
  numrows <- length(dataset)
  if(is.null(xlim))xlim<-c(1,numrows)
  d = data.frame(
    x  = c(1:numrows)
    , y  = dataset
    , xsd = data_std
  )

  if(is.null(ylab)) ylab<-"Data Value"
if(is.null(xlab)) xlab<-"Index"
  if(!lineonly){
    plot(d$x, d$y ,pch=pch, ylim= ylim,xlim= xlim,xlab=xlab, ylab=ylab,xaxt='n',yaxt='n',
         cex.lab=1.5, cex.axis=1.5,  cex.sub=1.5)
  with (
    data = d
    , expr = errbar(x, y, y+xsd, y-xsd, add=TRUE, pch=pch,type=type,col=col,lty=lty, cap=.01)
  )
  title(main=main,cex.main=cex.main)
  rtick<-xlim[1]
er<-xlim[2]
rtickinc<-round((er-rtick)/10)
if(rtickinc==0)rtickinc=1

axis(1,at=c(seq(rtick,er,by= rtickinc)),labels=rnames[seq(rtick,er,by=rtickinc)],lwd=2,cex.lab=1.5, cex.axis=1.5,  cex.sub=1.5)
axis(2,ylim= ylim,lwd=2,cex.lab=1.5, cex.axis=1.5,  cex.sub=1.5)
  } else {
  line(d$x, d$y )
  with (
    data = d
    , expr = errbar(x, y, y+xsd, y-xsd, add=TRUE, pch=pch,type=type,col=col,lty=lty, cap=.01)
  )
}

#legend("topright", legend = paste0("Slopes at Reference ", cnames[j]))
#
#   legend("topright", legend = paste0(vals$parameter," for ", cname))
#   lines(d$x,rep(0,numrows))
#   lines(d$x,rep(1,numrows))
  return()
}
