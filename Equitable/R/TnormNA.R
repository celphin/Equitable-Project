TnormNA <-
function(rmult,cmult,
                FUN=eg4,fac=0.5, noise=TRUE,NAfrac=NULL,
                diagonal=TRUE,Ave=TRUE,Zero=FALSE,imageplot=FALSE){
  d<-FUN(rmult,cmult)
  imagenan(d)
  if(noise){
    sdd<-sd(d,na.rm=TRUE)
    dn<-d+rnorm(prod(dim(d)),mean=0,sd=fac*sdd)    #normal distribution with std dev of fac*d's std dev

    dn<-matrix(dn,nrow=nrow(d),ncol=ncol(d))
    rownames(dn)<-rownames(d); colnames(dn)<-colnames(d)

    if(!is.null(NAfrac)){
    numspaces<-NAfrac*prod(dim(dn))
    cat("\nadding ",numspaces," NA values to ", prod(dim(dn))," total values")
    spaces<-sample(1:prod(dim(dn)), numspaces, replace=FALSE)

      #runif(numspaces, min = 1, max = prod(dim(dn)))  #uniform distribution between 0 and 1   std dev =sqrt((B-A)^2/12)
    dnNA<-dn
    dnNA[spaces]<-NA
    dNA<-d
    dNA[spaces]<-NA
    } else{
      dnNA<-NULL
      dNA<-NULL
    }
    if(imageplot){
    imagenan(d,main=paste0("Signal: rmult= ",rmult," cmult= ",cmult," fac=",fac))
      if(!is.null(NAfrac))imagenan(dNA,main=paste0("Signal with NAs : rmult= ",rmult," cmult= ",cmult,"\nfac=",fac, " fraction of NA=",NAfrac))
    imagenan(dn,main=paste0("Noise: rmult= ",rmult," cmult= ",cmult," fac=",fac))
    if(!is.null(NAfrac))imagenan(dnNA,main=paste0("Noise with NAs : rmult= ",rmult," cmult= ",cmult,"\nfac=",fac, " fraction of NA=",NAfrac))
    }
  }

  Td<-transformE(d,diagonal=diagonal,Ave=Ave,Zero=Zero)   #best?     most tests Sat night Nov 26
  cat("\n\nSTATISTICS OF SIGNAL\n")
  runstats(Td)
  if(!is.null(NAfrac)){
  TdNA<-transformE(dNA,diagonal=diagonal,Ave=Ave,Zero=Zero)
  cat("\n\nSTATISTICS OF SIGNAL WITH NAS\n")
  runstats(TdNA)
  # if(mean(TdNA$l.s.r2,na.rm=TRUE)!=1){
  # calc_pca(x=TdNA$smat,main=" on Original Data TdNA")
  # calc_pca(x=TdNA$ET.x,main=" on Equitable Transform TdNA")
  # }
  }
   Tdn<-transformE(dn,diagonal=diagonal,Ave=Ave,Zero=Zero)   #best?     most tests Sat night Nov 26
   cat("\n\nSTATISTICS OF SIGNAL WITH NOISE\n")
   runstats(Tdn)
   cat("\n\nSTATISTICS OF SIGNAL WITH NOISE COMPARED TO SIGNAL\n")
   runstatsNS(Td,Tdn)
   # if(mean(Tdn$l.s.r2,na.rm=TRUE)!=1){
   # calc_pca(x=Tdn$smat,main=" on Original Data Tdn")
   # calc_pca(x=Tdn$ET.x,main=" on Equitable Transform Tdn")
   # }
   if(!is.null(NAfrac)){
     TdnNA<-transformE(dnNA,diagonal=diagonal,Ave=Ave,Zero=Zero)
     cat("\n\nSTATISTICS OF SIGNAL WITH NOISE WITH NAS\n")
     runstats(TdnNA)

     cat("\n\nSTATISTICS OF SIGNAL WITH NOISE WITH NAS COMPARED TO SIGNAL WITH NAS\n")
     runstatsNS(TdNA,TdnNA)
     # if(mean(TdnNA$l.s.r2,na.rm=TRUE)!=1){
     # calc_pca(x=TdnNA$smat,main=" on Original Data TdnNA")
     # calc_pca(x=TdnNA$ET.x,main=" on Equitable Transform TdnNA")
     # }
     cat("\n\nSTATISTICS OF SIGNAL WITH NOISE WITH NAS COMPARED TO SIGNAL\n")
     runstatsNS(Td,TdnNA)
   } else{
     TdNA<-NULL
     TdnNA<-NULL
   }
 trans4<-list(Td=Td,Tdn=Tdn,TdNA=TdNA,TdnNA=TdnNA,rmult=rmult,cmult=cmult,fac=fac,NAfrac=NAfrac)
# plotsummary(Tdn)
 # plotsummary(TdnNA,Td)
 # imagenan(d,main=paste0("Signal: rmult= ",rmult," cmult= ",cmult," fac=",fac))
 # imagenan(dn,main=paste0("Noise: rmult= ",rmult," cmult= ",cmult," fac=",fac))
 # imagenan(dNA,main=paste0("Signal with NAs : rmult= ",rmult," cmult= ",cmult,"\nfac=",fac, " fraction of NA=",NAfrac))
 # imagenan(dnNA,main=paste0("Noise with NAs : rmult= ",rmult," cmult= ",cmult,"\nfac=",fac, " fraction of NA=",NAfrac))
  return(trans4)
}
