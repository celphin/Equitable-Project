eg3 <-
function(rmult,cmult){
  #example 3    f has no wave but u does
  cend<-360           #"space" multiplicative factor that increases number of  columns from standard number: rend
  rend<-300             #"time" multiplicative factor that increases number of rows  from standard number: cend
  rnum0<-15; cnum0<-10          # 45 times 10 space



  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  f<-(c/90)^(1/2)
  g<-sin(2*pi*(r+30)/300)+sin(2*pi*(r+30)/180)
  u<-0.5*c/360+10*sin(2*pi*c/720)

  Tx<- make_data(f,g,u,cend,rend,rnum0,cnum0,rmult,cmult)
  return(Tx)
}
