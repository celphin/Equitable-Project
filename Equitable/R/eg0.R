eg0 <-
function(rmult,cmult,n=NULL){
  if(is.null(n))n<-2
  #example 0
  cend<-10           #"space" multiplicative factor that increases number of  columns from standard number: rend
  rend<-360             #"time" multiplicative factor that increases number of rows  from standard number: cend
  rnum0<-15; cnum0<-10          # 10 times 15 space

  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  f<- rep(1,length(c))   #replicate 1 r times
  g<-sin(2*pi*(r+30)/900)
  #u<-c/10 +1
  u=abs(c-cend/2)^n/(cend/2)^n+1
  Tx<- make_data(f,g,u,cend,rend,rnum0,cnum0,rmult,cmult)
  return(Tx)
}
