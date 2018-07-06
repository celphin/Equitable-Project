egsep <-
function(rmult,cmult,Acx=1,mu=1,omega=1){
  cend<-360           #"space" multiplicative factor that increases number of  columns from standard number: rend
  rend<-360             #"time" multiplicative factor that increases number of rows  from standard number: cend
  rnum0<-15; cnum0<-10          # 15 times 10 space         length(r);length(c)
  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  f<-Acx*sin(mu*pi*(c)/cend)
  g<-cos(omega*pi*(r)/rend)
  u<-0*(c/cend+1)
  Tx<- make_data(f,g,u,cend,rend,rnum0,cnum0,rmult,cmult)
  return(Tx)
}
