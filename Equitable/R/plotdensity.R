plotdensity <-
function(s,
                      genname="Slope",num=NULL ){

 # if(is.null(ylimits)) ylimits<-c(min(c(I,orig,E,ls),na.rm=TRUE),max(I,na.rm=TRUE))
  if(is.null(num))num<-ncol(I)-1
  if(length(num)==1)listx<-seq(1,ncol(I), by=(ncol(I)-1)/num) else listx<-num
  cat(listx)
  sapply(listx,function(c){breaks<-100; mu<-summary(abs(s[c,]),na.rm=TRUE); msd<- sd(abs(s[c,]),na.rm=TRUE);
  mh<-hist(abs(s[c,]),prob=1,breaks=breaks,xlim=c(0,10),main=paste(genname,": row=",c),xlab="Slope value",ylim=c(0,1.5));
  den<-density(as.vector(abs(s[c,])),na.rm=TRUE)
  lines(den,col=2,lty=1 ,lwd=2)
  lines(rep( mu[2],200), seq(0.01,2, by=0.01),lty=4 ,lwd=2)
  lines(rep( mu[3],200), seq(0.01,2, by=0.01),lty=4 ,lwd=4)
  lines(rep( mu[5],200), seq(0.01,2, by=0.01),lty=4 ,lwd=2);
  lines(rep( mu[4],200), seq(0.01,2, by=0.01),lty=1 ,lwd=4)})

}
