plotsum4 <-
function(T4,row_unit=NULL,col_unit=NULL,z_unit=NULL,fpca=FALSE){

  cmult<-T4$cmult
  rmult<-T4$rmult
  fac<-T4$fac
  NAfrac<-T4$NAfrac
  Td<-T4$Td
  TdNA<-T4$TdNA
  Tdn<-T4$Tdn
  TdnNA<-T4$TdnNA
  if(!is.null(TdnNA)){
  plotsome(TdnNA,images=FALSE,signal=Td$smat,indiv=TRUE,of=TRUE,errb=TRUE,row_unit=row_unit,col_unit=col_unit,z_unit=z_unit )
  plotsome(TdnNA,images=FALSE,signal=Td$smat,transpose=TRUE,indiv=TRUE,of=TRUE,errb=TRUE,row_unit=row_unit,col_unit=col_unit,z_unit=z_unit )
  plotsome(TdnNA,signal=Td$smat,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE ,stderror=TRUE,row_unit=row_unit,col_unit=col_unit,z_unit=z_unit)#plots individual columns of Equitable Transform with std error of mean at each point
  plotsome(TdnNA,signal=Td$smat,transpose=TRUE,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE ,stderror=TRUE,row_unit=row_unit,col_unit=col_unit,z_unit=z_unit)#plots
  plotsome(TdnNA,signal=Td$smat,images=FALSE,versus=TRUE,of=TRUE,lf=TRUE,row_unit=row_unit,col_unit=col_unit,z_unit=z_unit)  #compares
  plotsome(TdnNA,signal=Td$smat,images=TRUE,of=TRUE,lf=TRUE,row_unit=row_unit,col_unit=col_unit,z_unit=z_unit)  #compares
  stats_residuals(TdnNA,Td=Td,genname="Equitable",ylim=NULL,ipf=FALSE,pf=FALSE)
  inc<-round(ncol(TdnNA$smat)/10)  # inc<-2
  if(inc<1)inc<-1
  nc<-seq(1,ncol(TdnNA$smat),by=inc)

  la<-which(colnames(TdnNA$l.s.pslope)=="Row_Ave")
  if(length(la)!=0){pm<-colMeans(TdnNA$l.s.pslope[,-la],na.rm=TRUE)
  }  else pm<-colMeans(TdnNA$l.s.pslope,na.rm=TRUE)
  refer<-which(pm==min(pm))
  cat("\nbest reference individual is ",colnames(TdnNA$l.s.pslope)[refer],"\n")
  if(length(refer)>1) refer<-refer[1]
  xvsrefplot(Td=Tdn,cgroup=nc,ref=refer,br=paste0( "Equitable Profiles with Min Probability Profile"))
  main<-paste("minp reference\n",colnames(TdnNA$l.s.pslope)[refer])
  plot_hist(TdnNA,refer=refer,main=main)

  main<-paste("\nEntire Matrix")
  plot_hist(TdnNA,main=main)

  if(NAfrac<0.76){
    findinfo(Tdave=TdnNA,printmax=FALSE,numb=1)   #if too many missing then uit fails
    bestintersectname<-a_b_bagplot(community.f=NULL,Td=TdnNA,refindex=1,main="TdnNA")
  }
  # nz<-findnonzerocolumns(x=Td_noise$smat)
  if(fpca){
    calc_pca(x=TdnNA$smat,main="PCA on Original Data")
   # calc_pca(x=TdnNA$ET.x,main="PCA on Equitable Transform")
  }

  }else{
    plotsome(Tdn,images=FALSE,signal=Td$smat,indiv=TRUE,of=TRUE,errb=TRUE ,row_unit=row_unit,col_unit=col_unit,z_unit=z_unit)
    plotsome(Tdn,images=FALSE,signal=Td$smat,transpose=TRUE,indiv=TRUE,of=TRUE,errb=TRUE,row_unit=row_unit,col_unit=col_unit,z_unit=z_unit )
    plotsome(Tdn,signal=Td$smat,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE ,stderror=TRUE,row_unit=row_unit,col_unit=col_unit,z_unit=z_unit)#plots individual columns of Equitable Transform with std error of mean at each point
    plotsome(Tdn,signal=Td$smat,transpose=TRUE,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE ,stderror=TRUE,row_unit=row_unit,col_unit=col_unit,z_unit=z_unit)#plots
    plotsome(Tdn,signal=Td$smat,images=FALSE,versus=TRUE,of=TRUE,lf=TRUE,row_unit=row_unit,col_unit=col_unit,z_unit=z_unit)  #compares
    inc<-round(ncol(Tdn$smat)/10)  # inc<-2
    if(inc<1)inc<-1
    nc<-seq(1,ncol(Tdn$smat),by=inc)

    la<-which(colnames(Tdn$l.s.pslope)=="Row_Ave")
    if(length(la)!=0){pm<-colMeans(Tdn$l.s.pslope[,-la],na.rm=TRUE)
    }  else pm<-colMeans(Tdn$l.s.pslope,na.rm=TRUE)
    refer<-which(pm==min(pm))
    cat("\nbest reference individual is ",colnames(Tdn$l.s.pslope)[refer],"\n")
    if(length(refer)>1) refer<-refer[1]
    xvsrefplot(Td=Tdn,cgroup=nc,ref=refer,br=paste0( "Equitable Profiles with Min Probability Profile")) # ,numb=10   Feb15 2018
    main<-paste("minp reference\n",colnames(Tdn$l.s.pslope)[refer])
    plot_hist(Tdn,refer=refer,main=main)

    main<-paste("\nEntire Matrix")
    plot_hist(Tdn,main=main)
    # nz<-findnonzerocolumns(x=Td_noise$smat)

     findinfo(Tdave=Tdn,printmax=FALSE,numb=1)   #if too many missing then uit fails
      bestintersectname<-a_b_bagplot(community.f=NULL,Td=Tdn,refindex=1,main="Tdn")

    if(mean(Tdn$l.s.r2,na.rm=TRUE)<0.97){
      calc_pca(x=Tdn$smat,main="PCA on Original Data")
      calc_pca(x=Tdn$ET.x,main="PCA on Equitable Transform")
    }

     }

  if(!is.null(TdnNA$ET.x))imagenan(TdnNA$ET.x,main=paste0("ET from Signal+Noise with NAs : rmult= ",
                                                          rmult," cmult= ",cmult,"\nfac=",fac, " fraction of NA=",NAfrac),row_unit=row_unit,col_unit=col_unit,zlim=z_unit)
  if(!is.null(TdnNA$smat))imagenan(TdnNA$smat,main=paste0("Signal+Noise with NAs : rmult= ",
                                                          rmult," cmult= ",cmult,"\nfac=",fac, " fraction of NA=",NAfrac),row_unit=row_unit,col_unit=col_unit,zlim=z_unit)
  if(!is.null(TdNA$smat))imagenan(TdNA$ET.x,main=paste0("ET from Signal with NAs: rmult= ",
                                                        rmult," cmult= ",cmult," fraction of NA=",NAfrac),row_unit=row_unit,col_unit=col_unit,zlim=z_unit)
  if(!is.null(TdNA$smat))imagenan(TdNA$smat,main=paste0("Signal with NAs: rmult= ",
                                                        rmult," cmult= ",cmult," fraction of NA=",NAfrac),row_unit=row_unit,col_unit=col_unit,zlim=z_unit)
  imagenan(Tdn$ET.x,main=paste0("ET from Signal+Noise: rmult= ",rmult," cmult= ",cmult,"\nfac=",fac),row_unit=row_unit,col_unit=col_unit,zlim=z_unit)
  imagenan(Tdn$smat,main=paste0("Signal+Noise: rmult= ",rmult," cmult= ",cmult,"\nfac=",fac),row_unit=row_unit,col_unit=col_unit,zlim=z_unit)
  if(!is.null(Td$smat))imagenan(Td$smat,main=paste0("Signal : rmult= ",rmult," cmult= ",cmult),row_unit=row_unit,col_unit=col_unit,zlim=z_unit)






}
