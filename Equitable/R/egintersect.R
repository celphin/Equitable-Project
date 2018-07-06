egintersect <-
function(rmult,cmult){
  cend<-360           #"space" multiplicative factor that increases number of  columns from standard number: rend

  rnum0<-15; cnum0<-10          # 15 times 10 space        length(r);length(c)       plot(f+u)     plot(g)
  rend<-rnum0*rmult            #"time" multiplicative factor that increases number of rows  from standard number: cend


  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  f<-(c-180)/360+1
  azero<-(rend/2-rend/20)
  g<-(r-azero)^3/(azero)^3*20

  u<-rep(200,length(c))
  cat("\n intersection set to ", azero," with value of ",f[1]*g[azero]+u[1],"\n")
  F<-matrix(rep(f,2),nrow=length(f),ncol=2)
  imagenan(t(F),main="Function f(x)")
  U<-matrix(rep(u,2),nrow=length(u),ncol=2)
  #imagenan(t(U),main="Function u(x)")
  G<-matrix(rep(g,2),nrow=length(g),ncol=2)
  imagenan(G,main="Function g(t)")
  Tx<- make_data(f,g,u,cend,rend,rnum0,cnum0,rmult,cmult)
  return(Tx)
}
