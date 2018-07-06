eg2 <-
function(rmult,cmult){
  #example 2  wave in f only
  cend<-360           #"space" multiplicative factor that increases number of  columns from standard number: rend
  rend<-15             #"time" multiplicative factor that increases number of rows  from standard number: cend
  rnum0<-15; cnum0<-10          # 15 times 10 space

  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  f<-sin(2*pi*(c)/180)
  g<-1/5*r
  u<-c/90
  Tx<- make_data(f,g,u,cend,rend,rnum0,cnum0,rmult,cmult)
  return(Tx)
}
