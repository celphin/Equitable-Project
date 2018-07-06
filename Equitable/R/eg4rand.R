eg4rand <-
function(rmult,cmult){
  cend<-360           #"space" multiplicative factor that increases number of  columns from standard number: rend
  rend<-360             #"time" multiplicative factor that increases number of rows  from standard number: cend
  rnum0<-15; cnum0<-10          # 15 times 10 space         length(r);length(c)
  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  f<-((c/cend)^(1/2)+3)*sin(2*pi*(c+20)/720)
  g<-sin(2*pi*(r+30)/270)+sin(2*pi*(r+30)/180)
  u<-3*(c/cend+1)
  Tx<- make_data(f,g,u,cend,rend,rnum0,cnum0,rmult,cmult)
  newcol<-sample(1:ncol(Tx), ncol(Tx), replace=F)
  newrow<-sample(1:nrow(Tx), nrow(Tx), replace=F)
  Tx[,newcol]<-Tx[,1:ncol(Tx)]
  Tx[newrow,]<-Tx[1:nrow(Tx),]
  return(Tx)
}
