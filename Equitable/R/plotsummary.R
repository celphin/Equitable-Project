plotsummary <-
function(Td_noise,Td=NULL,Td_old=NULL,
                      row_unit=NULL,col_unit=NULL,z_unit=NULL,
                      yline=3,yma=5,fintersect=FALSE,fsquares=FALSE,fpca=FALSE,fave=FALSE){
  if(!is.null(Td))runstatsNS(Td,Td_noise)
  plotsquares(Td_noise, signal=Td,of=TRUE,
              row_unit=row_unit,col_unit=col_unit,z_unit=z_unit) #Show images of slopes and intercepts
  if(fsquares){
  plotsquares(Td_noise, signal=Td,transpose=TRUE,images=FALSE,columns=TRUE,of=TRUE,
              row_unit=row_unit,col_unit=col_unit,z_unit=z_unit) #all rows plotted together

  plotsquares(Td_noise,signal=Td,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE,
              row_unit=row_unit,col_unit=col_unit,z_unit=z_unit) #with least sq, fits can put limits on,slimits=c(0,3) ,blimits=c(-10,+10)
  plotsquares(Td_noise,signal=Td,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE,stderror=TRUE,lf=TRUE,
              row_unit=row_unit,col_unit=col_unit,z_unit=z_unit) #with signal
  }
  plotsome(T=Td_noise,signal=Td$smat,transpose=TRUE,images=FALSE,indiv=TRUE,of=TRUE,lf=TRUE,errb=TRUE,
           row_unit=row_unit,col_unit=col_unit,z_unit=z_unit ) # could use vector num<-c(1,10,11,15)
  plotsome(T=Td_noise,signal=Td$smat,images=FALSE,indiv=TRUE,of=TRUE,lf=TRUE,errb=TRUE,
           row_unit=row_unit,col_unit=col_unit,z_unit=z_unit )
  plotsome(T=Td_noise,signal=Td$smat,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE ,stderror=TRUE,
           row_unit=row_unit,col_unit=col_unit,z_unit=z_unit) #plots individual columns of Equitable Transform with std error of mean at each point
  plotsome(T=Td_noise,signal=Td$smat,transpose=TRUE,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE ,stderror=TRUE,
           row_unit=row_unit,col_unit=col_unit,z_unit=z_unit)#plots individual rows of Equitable Transform with std error of mean at each point
#   plotsome(T=Td_noise,signal=Td$smat,num=c(132,123,99,92,77),transpose=TRUE,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE ,stderror=TRUE,row_unit=row_unit,col_unit=col_unit,z_unit=z_unit)#plots individual rows of Equ
  plotsome(T=Td_noise,signal=Td$smat,transpose=TRUE,images=FALSE,columns=TRUE,of=TRUE,lf=TRUE,
           row_unit=row_unit,col_unit=col_unit,z_unit=z_unit) #plots all columns together for original, least squares,signal and equitable
  plotsome(T=Td_noise,signal=Td$smat,images=FALSE,columns=TRUE,of=TRUE,lf=TRUE,row_unit=row_unit,
           col_unit=col_unit,z_unit=z_unit) #plots all columns together for original, least squares,signal and equitable
  stats_residuals(Td_noise,Td=Td,Td_old=Td_old,genname="Equitable",ylim=NULL,ipf=FALSE,pf=FALSE)

  inc<-round(ncol(Td_noise$smat)/10)  # inc<-2
  if(inc<1)inc<-1
  nc<-seq(1,ncol(Td_noise$smat),by=inc)

  la<-which(colnames(Td_noise$l.s.pslope)=="Row_Ave")    #Td$l.s.pslope[which(is.nan(Td$l.s.pslope))]<-NA
  if(length(la)!=0){pm<-colMeans(Td_noise$l.s.pslope[,-la],na.rm=TRUE)
  }  else pm<-colMeans(Td_noise$l.s.pslope,na.rm=TRUE)
  refer<-which(pm==min(pm,na.rm=TRUE))

  if(length(refer)>0)cat("\nbest reference individual is ",colnames(Td_noise$l.s.pslope)[refer],"\n") else{
    cat("\n no minp reference found: reset to 1\n")
    refer<-1
  }
  if(length(refer)>1) refer<-refer[1]
  xvsrefplot(Td=Td_noise,cgroup=nc,ref=refer,br=paste0( "Equitable Profiles with Min Probability Profile"))
  main<-paste("minp reference\n",colnames(Td_noise$l.s.pslope)[refer])
  plot_hist(Td_noise,refer=refer,main=main)
  # main<-paste("\nEntire Matrix")
  # plot_hist(Td_noise,main=main)

  if(length(la)==1)xvsrefplot(Td=Td_noise,cgroup=nc,ref="Row_Ave",br="Td_noise")   #compare profiles with either Row_Ave or  column with largest range
 # xvsrefplot(Td=Td_noise,cgroup=nc,ref=132,br="Td_noise")

  if(fintersect) {
    findinfo(Tdave=Td_noise,printmax=FALSE,numb=1)    #findinfo(Tdave=Td,printmax=FALSE,numb=1,slim=c(-2,2))
                                                        #findinfo(Tdave=Td,printmax=FALSE,numb=1)

  }
  if(fpca){


  # nz<-findnonzerocolumns(x=Td_noise$smat)  #mean(Td_noise$l.s.pslope,na.rm=TRUE)
   if(mean(Td_noise$l.s.r2,na.rm=TRUE)<0.95){
    r2<-Td_noise$l.s.r2
    removec<-unlist(sapply(1:ncol(r2),function(c){if(length(r2[is.na(r2[,c]),c])==nrow(r2))return(c)}))
    if(!is.null(removec))r2<-r2[-removec,-removec]    #Td_noise<-Tdall    Td_noise<-Tday
    calc_pca(x=r2,main="PCA on R2") #calc_pca(x=r2,main="PCA on Original Data",fcol=TRUE,col=kcl$cluster)
   } else cat("\nNo pca for r2 on Original Data: Perfect fit\n")
    smat<-Td_noise$smat  #imagenan(smat)
    removec<-unlist(sapply(1:ncol(smat),function(c){if(length(smat[is.na(smat[,c]),c])>=0.6*nrow(smat))return(c)}))
    if(!is.null(removec))smat<-smat[-removec,-removec]    #Td_noise<-Tdall    Td_noise<-Tday
    calc_pca(x=smat,main="PCA on Original Data")
  # if(mean(Td_noise$l.s.pslope,na.rm=TRUE)>0.0003){    # 0.03
    ETx<-Td_noise$ET.x  #imagenan(smat)
    removec<-unlist(sapply(1:ncol(ETx),function(c){if(length(ETx[is.na(ETx[,c]),c])>=0.6*nrow(ETx))return(c)}))
    if(!is.null(removec))ETx<-ETx[-removec,-removec]    #Td_noise<-Tdall    Td_noise<-Tday
    calc_pca(x=ETx,main="PCA on Equitable Transform")
  # } else cat("\nNo pca on Equitable Transform\n")
  }
  if(fintersect){
    if(length(la)!=0 )
    bestintersectname<-a_b_bagplot(community.f=NULL,
                                   Td=Td_noise,refindex=la) else bestintersectname<-a_b_bagplot(community.f=NULL,Td=Td_noise,refindex=1)
    }

  if(fave){
    plotAveprofiles(Td96=Td_noise, main="Td_Noise Data")
  if(!is.null(Td))plotAveprofiles(Td96=Td, main="Td Signal Data",xlim=row_unit,ylim=col_unit)
}
   plotsome(Td_noise,images=FALSE, xvsref=ncol(Td_noise$smat),
            row_unit=row_unit,col_unit=col_unit,z_unit=z_unit )  #plot all columns from Ave contructed transform versus the Average (reference column)
  plotsome(T=Td,images=FALSE, xvsref=ncol(Td$smat),
           row_unit=row_unit,col_unit=col_unit,z_unit=z_unit ) #plot all Signal columns from Ave contructed transform versus the Average (reference column)
  plotsome(T=Td_noise,signal=Td$smat,images=FALSE,versus=TRUE,of=TRUE,lf=TRUE,
           row_unit=row_unit,col_unit=col_unit,z_unit=z_unit)  #compares
 #could put in error bars for versuws type plots
  plotsome(T=Td_noise, signal=Td$smat,of=TRUE,lf=TRUE,errb=TRUE,
           row_unit=row_unit,col_unit=col_unit,z_unit=z_unit,yma=yma,yline=yline)     #images and contours of Equitable transform
}
