Tnormrow <-
function(rmult,cmult,
                   FUN=eg4,fac=0.5, noise=TRUE,
                   diagonal=TRUE){
  d<-FUN(rmult,cmult)
  if(noise){
    d_noise<-matrix(d,nrow=nrow(d),ncol=ncol(d))
    sdd<-sd(d,na.rm=TRUE)
    for(row in 1:nrow(d)){
      d_noise[row,]<-d[row,]+rnorm(ncol(d),mean=0,sd=fac*sdd)    #normal distribution with std dev of fac*d's std dev
    }
    d_noise<-matrix(d_noise,nrow=nrow(d),ncol=ncol(d))
    rownames(d_noise)<-rownames(d); colnames(d_noise)<-colnames(d)
    d<-d_noise
    imagenan(d,main=paste0("Noise: rmult= ",rmult," cmult= ",cmult," fac=",fac))
  }
  #Td_noise<-transformE(d_noise,minp=0.5,mins=0,minfinal=1/5, Ave=TRUE)   #best?
  Td<-transformE(d, Ave=TRUE,diagonal=diagonal)   #best?     most tests Sat night Nov 26
  # Td<-transformE(d,minp=0.5,mins= 0,minfinal=1/15, Ave=TRUE)   #best?   #used a great deal for development Nov 27
  # Td_noise<-transformE(d_noise,minp=0.7,mins=0,minfinal=1/5)
  runstats(Td)
  return(Td)
}
