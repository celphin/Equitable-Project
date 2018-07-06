eg02 <-
function(rmult,cmult,freq=1){
  #example 0
  cend<-360           #"space" multiplicative factor that increases number of  columns from standard number: rend
  rend<-360             #"time" multiplicative factor that increases number of rows  from standard number: cend
  rnum0<-15; cnum0<-10          # 10 times 15 space

  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  f<- 1+cos(2*pi*1/2*(c)/360)  #
  g<-cos(2*pi*freq*(r)/360)+0.5
  u<-c*0
  Tx<- make_data(f,g,u,cend,rend,rnum0,cnum0,rmult,cmult)
  return(Tx)
}
