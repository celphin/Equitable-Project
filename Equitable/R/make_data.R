make_data <-
function(f,g,u,
                    cend,rend,rnum0,cnum0,rmult,
                    cmult,pf=TRUE){
  #run below code for each example
  t<-1:(rnum0*rmult) ; x<-1:(cnum0*cmult)
  d<-outer(t,x,Vectorize(FUN=function(t,x,f,g,u){f[x]*g[t]+u[x]} )
           ,f=as.data.frame(f),u=as.data.frame(u),g=as.data.frame(g))
  rownames(d)<-t; colnames(d)<-x
  if(pf && nrow(d) >1 && ncol(d)>1)imagenan(d,main=paste0("SIGNAL: rmult= ",rmult," cmult= ",cmult))
  return(d)
}
