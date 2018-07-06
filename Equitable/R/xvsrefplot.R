xvsrefplot <-
function(Td,cgroup=NULL,ylim=NULL,ref=NULL,br="",pf=TRUE,extranames=NULL,err95=TRUE,fitl=TRUE){
  #if(is.null(ref))ref<-ncol(Td$ET.x)
  if(!is.null(ref)){
    nref<-which(colnames(Td$smat)=="Row_Ave")[1]
    if( ref=="Row_Ave"){
 if(length(nref)!=0 )ref<-nref else ref<-NULL
    }
  }
  if(is.null(ylim)){
    mins<-min(Td$smat[,cgroup],na.rm = TRUE)
    maxs<-max(Td$smat[,cgroup],na.rm = TRUE)
    ds<-maxs-mins
    if(nrow(Td$ET.x)<=21) ylim<-c(mins-1*ds*3/8,maxs+ds*3/8) else ylim<-c(mins-1*ds*3/8,maxs+ds*0/8)

    if((ylim[2]-ylim[1])<1e-10)ylim<-c(0,1)
    }
   yrange<-NULL
   if(is.null(cgroup))cgr<-1:ncol(Td$ET.x) else cgr<-cgroup
    for(c in cgr){yrange<-c(yrange,abs(min(Td$ET.x[,c],na.rm = TRUE)-max(Td$ET.x[,c],na.rm = TRUE)))}
    ref1<-cgr[which(max(yrange,na.rm=TRUE)==yrange)]
    ref1<-ref1[1]
    if(is.null(ref1)){cat("\nref not set :initializing to 1");ref<-ref1<-1}
  if(!is.null(ref)) {
    refrange<-abs(min(Td$ET.x[,ref],na.rm = TRUE)-max(Td$ET.x[,ref],na.rm = TRUE))
    if(refrange/abs(ylim[2]-ylim[1])<0.1 ||is.null(ref))ref<-ref1
  } else ref<-ref1
  rmean<-Td$ET.x[,ref]
  rmeano<-Td$smat[,ref]
  for(c in cgroup) {
    #cat("\n",c)
    y<-Td$ET.x[,c]
    yo<-Td$smat[,c]
    #cat("\n",y)
    stdy<-Td$ET.xsd[,c]
    ps<-Td$l.s.pslope[c,ref]
    r2<-Td$l.s.r2[c,ref]
    s<-Td$E.s[c,ref]
    b<-Td$E.b[c,ref]

    if(err95){
    se<-2*Td$E.sd1[c,ref]/sqrt(Td$E.sN[c,ref])
    be<-2*Td$E.bsd1[c,ref]/sqrt(Td$E.bN[c,ref])
    sN<-Td$E.sN[c,ref]
    bN<-Td$E.bN[c,ref]
    } else {
      se<-NULL
      be<-NULL
      sN<-NULL
      bN<-NULL
    }
    #refname and cname are names of varianble that wiill be displayed to show both needs shrinkage of font
    if(!is.numeric(ref))gref<-which(colnames(Td$ET.x)==ref) else gref<-ref
    if(pf){
    if( fitl){
      if(is.null(extranames)) plotp1vsp2(rmean=rmean,rmeano=rmeano,y=y,yo=yo,stdy=stdy,ylim=ylim,ps=ps,r2=r2,s=s,b=b,
                                         se=se,be=be,sN=sN,bN=bN,
                                         br=paste0(br),
                                         refname=colnames(Td$ET.x)[gref],cname=colnames(Td$ET.x)[c]) else {
                                           plotp1vsp2(rmean=rmean,rmeano=rmeano,y=y,yo=yo,stdy=stdy,ylim=ylim,ps=ps,r2=r2,s=s,b=b,
                                                      se=se,be=be,sN=sN,bN=bN,
                                                      br=paste0(br),
                                                      refname=extranames[gref],cname=extranames[c])
                                         }
    } else {
      if(is.null(extranames)) plot1vs2(rmeano=rmeano,yo=yo,ylim=ylim,
                                         br=paste0(br),
                                         refname=colnames(Td$ET.x)[gref],cname=colnames(Td$ET.x)[c]) else {
                                           plot1vs2(rmeano=rmeano,yo=yo,ylim=ylim,
                                                      br=paste0(br),
                                                      refname=extranames[gref],cname=extranames[c])
                                         }
    }
  }
  }
  return(ref1)
}
