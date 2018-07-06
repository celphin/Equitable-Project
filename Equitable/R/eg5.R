eg5 <-
function(rmult,cmult){
  cend<-360           #"space" multiplicative factor that increases number of  columns from standard number: rend
  rend<-360             #"time" multiplicative factor that increases number of rows  from standard number: cend
  rnum0<-15; cnum0<-10          # 15 times 10 space        length(r);length(c)       plot(f+u)     plot(g)



  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  f<-(c)^(1/2)*sin(2*pi*(c+20)/90)
  g<-sin(2*pi*(r+30)/180)+sin(2*pi*(r+30)/90)
  u<-0.1*c+10*sin(2*pi*c/60)
  F<-matrix(rep(f,2),nrow=length(f),ncol=2)
  imagenan(t(F),main="Function f(x)")
  U<-matrix(rep(u,2),nrow=length(u),ncol=2)
  imagenan(t(U),main="Function u(x)")
  G<-matrix(rep(g,2),nrow=length(g),ncol=2)
  imagenan(G,main="Function g(t)")
  Tx<- make_data(f,g,u,cend,rend,rnum0,cnum0,rmult,cmult)
  return(Tx)
}
