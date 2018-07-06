eg6 <-
function(rmult,cmult){
  cend<-360           #"space" multiplicative factor that increases number of  columns from standard number: rend
  rend<-15             #"time" multiplicative factor that increases number of rows  from standard number: cend
  rnum0<-15; cnum0<-10          # 10 times 15 space


  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  f<- 1 *tan(pi*(c/(cend+1)))
  g<-r
  u<-c*0


  Tx<- make_data(f,g,u,cend,rend,rnum0,cnum0,rmult,cmult)
  return(Tx)
}
