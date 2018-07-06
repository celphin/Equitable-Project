rows_via_intercept <-
function(Td_noise,rowlist=NULL,pnum=3,ref=NULL,z_unit=NULL,AVE=TRUE){

  if(is.null(rowlist))rowlist<-seq( nrow(Td_noise$smat),1, by=(-1*nrow(Td_noise$smat)/pnum))

  if(is.null(ref))ref<-ncol(Td_noise$smat)
  sdf<-1.4*(max(Td_noise$ET.Eb[,ref],na.rm=TRUE)-min(Td_noise$ET.Eb[,ref],na.rm=TRUE))
  for( rownum in rowlist){
    Tdnew<-reviseb_witherror(rownum=rownum,Td_noise,ref=ref)
    #sdf<-1.1*(max(Tdnew$ET.Eb[,ref],na.rm=TRUE)-min(Tdnew$ET.Eb[,ref],na.rm=TRUE))
    zero<-Tdnew$l.s.zero
    #num<-c((ref-1):ref)
    num<-c((rownum),rownum)
    if(AVE){
      limits=c(zero-sdf,zero+sdf)
      blimits=c(-sdf,+sdf)
      }else {
      limits=c(zero-sdf,zero)
      blimits=c(-sdf,0.3*sdf)
    }
    plotsome(Tdnew,images=FALSE,indiv=TRUE,num=num,transpose=TRUE,errb=TRUE,stderror=TRUE, of=TRUE,limits=limits,
             genname=paste0("Row value=",floor(rownum)," zero= ",round(zero,digits=1),"\n"),
             row_unit="", col_unit=paste0("Data Column)"),z_unit=z_unit) #images and c
    num<-c(ref,ref)
    plotsquares(Tdnew,num=num,images=FALSE,indiv=TRUE, of=FALSE,errb=TRUE,stderror=TRUE,slimits=c(0,1.2) ,blimits=blimits,
                main=paste0("Row value=",floor(rownum),"\nzero= ",round(zero,digits=1)),psf=FALSE,
                row_unit="", col_unit=paste0("Column (Row value= ",floor(rownum)," zero= ",round(zero,digits=1)),z_unit=z_unit)

     num<-c((rownum),rownum)
    plotsome(Tdnew,images=FALSE,indiv=TRUE,num=num,transpose=TRUE,errb=TRUE, of=TRUE,limits=limits,
             genname=paste0("Row value=",floor(rownum)," zero= ",round(zero,digits=1),"\n"),
             row_unit="", col_unit=paste0("Data Column)"),z_unit=z_unit) #images and c
    num<-c(ref,ref)
    plotsquares(Tdnew,num=num,images=FALSE,indiv=TRUE, of=FALSE,errb=TRUE,slimits=c(0,1.2) ,blimits=blimits,
                main=paste0("Row value=",floor(rownum),"\nzero= ",round(zero,digits=1)),psf=FALSE,
                row_unit="", col_unit=paste0("Column (Row value= ",floor(rownum)," zero= ",round(zero,digits=1)),z_unit=z_unit)

  }

}
