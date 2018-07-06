plotgroup_with_errors <-
function(smatave,smatsd,main="",xlim=NULL,ylim=NULL,xlab="ROW",ylab="DATA VALUE",inames=NULL,
                                leg=TRUE,maxnum=8,cex=NULL,cex.main=0.85){
  if(is.null(cex))cex=0.8
  if(is.null(inames))inames<-colnames(smatave)

  for (firstnum in seq(1,ncol(smatave), by=maxnum)){

  if((firstnum+maxnum-1)<=ncol(smatave))endval<-(firstnum+maxnum-1) else endval<-ncol(smatave)
  plotdata_with_errors( smatave[,firstnum],smatsd[,firstnum],rnames=rownames(smatave),
                        main=paste(main,(firstnum-1)/maxnum),xlim=NULL,
                        xlab=xlab,ylab=ylab,ylim=ylim,pch=16,col=3,cex.main=cex.main)
  for (j in firstnum:endval){    #  for (j in 1:ncol(smatave)){ cat(" ",j%%5+1 ) }  for (j in 1:ncol(smatave)){ cat(" ",j,j%%5 ) }
    jp<-(j-firstnum+1)%%11
    jc<-(j-firstnum+1)
    jt<-(j-firstnum+1)%%5
    plotdata_with_errors( smatave[,j],smatsd[,j],rnames=rownames(smatave),
                          main=paste(main,(firstnum-1)/maxnum),xlim=NULL,xlab=xlab,ylab=ylab,ylim=ylim,
                          pch=(jp+15),type="b",col=(jc+2),lty=jt+1,lineonly=TRUE,cex.main=cex.main)
  }
  if(leg)legend("topleft",inset= c(0,0.0),paste(inames[firstnum:(endval)]),
                pch=(1:(endval-firstnum+1)%%11+15) ,
                col=(1:(endval-firstnum+1)+2) ,
                lty=(1:(endval-firstnum+1)%%5+1) ,cex=cex,pt.cex=1)

}
}
