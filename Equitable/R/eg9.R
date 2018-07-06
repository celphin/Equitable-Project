eg9 <-
function(rmult,cmult){
  cend<-10           #"space" multiplicative factor that increases number of  columns from standard number: rend
  rend<-15             #"time" multiplicative factor that increases number of rows  from standard number: cend
  rnum0<-15; cnum0<-10          # 15 times 10 space        length(r);length(c)       plot(f+u)     plot(g)



  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  set.seed(10)
  f<- rep(1,length(c))   #replicate 1 r times
  set.seed(1330)
  g<-rnorm(length(r),mean=1,sd=1)
  set.seed(11130)
  u<-rnorm(length(c),mean=5,sd=3)
  set.seed(NULL)
  Tx<- make_data(f,g,u,cend,rend,rnum0,cnum0,rmult,cmult)
  return(Tx)
}
