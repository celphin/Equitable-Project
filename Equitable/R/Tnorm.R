Tnorm <-
function(rmult,cmult,
                FUN=eg4,fac=0.5, noise=TRUE,
                diagonal=TRUE,Ave=TRUE,Zero=FALSE){
  d<-FUN(rmult,cmult)
  if(noise){
    sdd<-sd(d,na.rm=TRUE)
    d_noise<-d+rnorm(prod(dim(d)),mean=0,sd=(fac*sdd))    #normal distribution with std dev of fac*d's std dev
    #d_noise<-jitter(c(d), factor = 5, amount = 0)
    d_noise<-matrix(d_noise,nrow=nrow(d),ncol=ncol(d))
    rownames(d_noise)<-rownames(d); colnames(d_noise)<-colnames(d)
    cat(sd(d),sd(d_noise),sd(d-d_noise))
    d<-d_noise
    imagenan(d,main=paste0("Noise: rmult= ",rmult," cmult= ",cmult," fac=",fac))

  }

  Td<-transformE(d,diagonal=diagonal,Ave=Ave,Zero=Zero)   #best?     most tests Sat night Nov 26

  runstats(Td)
  return(Td)
}
