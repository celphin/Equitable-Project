eg7 <-
function(rmult,cmult){
  cend<-360           #"space" multiplicative factor that increases number of  columns from standard number: rend
  rend<-360             #"time" multiplicative factor that increases number of rows  from standard number: cend
  rnum0<-15; cnum0<-10          # 10 times 15 space


  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  f<- abs(c-cend/2)+20
  g<-ceiling(r/(rend/5))+1
  u<-0*c
  F<-matrix(rep(f,2),nrow=length(f),ncol=2)
  imagenan(t(F),main="Function f(x)")
  # U<-matrix(rep(u,2),nrow=length(u),ncol=2)
  # imagenan(t(U),main="Function u(x)")
  G<-matrix(rep(g,2),nrow=length(g),ncol=2)
  imagenan(G,main="Function g(t)")
  cat("\n std f ",sd(f),"   std g ",sd(g),"   std u ",sd(u), "    std(f)std(g) ",sd(f)*sd(g))
  cat("\n sqrt mean (f^2) ",sqrt(mean(f^2)),"   sqrtmean (g^2) ",sqrt(mean(g^2)),"  sqrtmean (u^2) ",sqrt(mean(u^2)), "    sqrt(mean(f^2))*std(g) ",sqrt(mean(f^2))*sd(g))

  Tx<- make_data(f,g,u,cend,rend,rnum0,cnum0,rmult,cmult)
  return(Tx)
}
