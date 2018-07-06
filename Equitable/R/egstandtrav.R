egstandtrav <-
function(rmult,cmult){
  cend<-10           #"space" multiplicative factor that increases number of  columns from standard number: rend
  rend<-10             #"time" multiplicative factor that increases number of rows  from standard number: cend
  rnum0<-10; cnum0<-10          # 15 times 10 space        length(r);length(c)       plot(f+u)     plot(g)

  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  f<- c*1
  g<-r*1
  u<-c*1
  A<- make_data(f,g,u,cend,rend,rnum0,cnum0,rmult,cmult)

  ti<-t(1:(rnum0*rmult)) ; x<-t(1:(cnum0*cmult))
  d<-matrix(NA,nrow=length(ti),ncol=length(x))

  for(a in ti){
    for(b in x){
      #cat(a,b,"\n")
     # d[a,b]<-30*sin(2*pi*(r[a]+c[b])/(rend))
      d[a,b]<-sin(2*pi*(r[a]+c[b]))

    }
  }
  d<-4*d+A
  rownames(d)<-ti; colnames(d)<-x
  imagenan(d,main=paste0("SIGNAL: rmult= ",rmult," cmult= ",cmult))
  Tx<- d
  return(Tx)
}
