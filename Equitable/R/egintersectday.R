egintersectday <-
function(rmult,cmult,days_between_sample=0,amp=40,nfac=0,g0=0,A=200){
  cend<-360           #"space" multiplicative factor that increases number of  columns from standard number: rend

  rnum0<-15; cnum0<-10          # 15 times 10 space        length(r);length(c)       plot(f+u)     plot(g)
  rend<-rnum0*rmult            #"time" multiplicative factor that increases number of rows  from standard number: cend


  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  f<-(c-180)/360+1
  azero<-(rend/2-rend/20)
  g<-(r-azero)^3/(azero)^3*amp+g0

  u<-rep(A,length(c))
  cat("\n intersection set to ", azero," with value of ",f[1]*g[azero]+u[1]," but amp is ",amp," displaced by g0=",g0," and A=",A,"\n")
  F<-matrix(rep(f,2),nrow=length(f),ncol=2)
  imagenan(t(F),main="Function f(x)")
  U<-matrix(rep(u,2),nrow=length(u),ncol=2)
  #imagenan(t(U),main="Function u(x)")
  G<-matrix(rep(g,2),nrow=length(g),ncol=2)
  imagenan(G,main="Function g(t)")
  Tx<- make_data(f,g,u,cend,rend,rnum0,cnum0,rmult,cmult)
  sdd<-sd(Tx,na.rm=TRUE)
  d_noise<-Tx+rnorm(prod(dim(Tx)),mean=0,sd=nfac*sdd)    #normal distribution with std dev of fac*d's std dev
  Tx<-matrix(d_noise,nrow=nrow(Tx),ncol=ncol(Tx))
  if(days_between_sample!=0){
  Tx<-days_between_sample*ceiling(Tx/days_between_sample)
  }
  rownames(Tx)<-r
  colnames(Tx)<-c
  imagenan(Tx,main="Data (x,t) resolution (days) for sampling=", days_between_sample)
  # for(c in 1:nrow(Tx)){
  #   for(r in 1:ncol(Tx)){
  #    Tx[r,c]<-4*ceiling(Tx[r,c]/4)
  #   }
  # }
  return(Tx)
}
