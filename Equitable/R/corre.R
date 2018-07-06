corre <-
function(matr){
  nc<-ncol(matr)
    val<-sapply(1:nc,FUN=runy,mat=matr)
    val1<-array(as.numeric(val),dim=c(nc,9,nc)); colnames(val)<-colnames(matr)
    m<-list(
    s=t(val1[,1,]),
    sse=t(val1[,2,]),
    b=t(val1[,3,]),
    bse=t(val1[,4,]),
    r2=t(val1[,5,]),
    N=t(val1[,6,]),
    pslope=t(val1[,7,]),
    node=t(val1[,8,]),
    p_par=t(val1[,9,])
   )
    m$pslope[which(is.nan(m$pslope))]<-NA
    if(length(colnames(matr))!=0) rown<-paste0("y_",colnames(matr)) else rown<-colnames(matr)
    coln<-colnames(matr)
    rownames(m$s)<-rownames(m$sse)<-rownames(m$b)<-rownames(m$bse)<-rownames(m$r2)<-rownames(m$N)<-rownames(m$pslope)<-rownames(m$node)<-rownames(m$p_par)<-rown
    colnames(m$s)<-colnames(m$sse)<-colnames(m$b)<-colnames(m$bse)<-colnames(m$r2)<-colnames(m$N)<-colnames(m$pslope)<-colnames(m$node)<-colnames(m$p_par)<-coln
  return(m)
}
