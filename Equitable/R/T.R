T <-
function(rmult,cmult,
            FUN=eg4, noise=TRUE,diagonal=TRUE){
  d<-FUN(rmult,cmult)
  if(noise){
    d_noise<-jitter(c(d), factor = 5, amount = 0)
    #jitter same as runif :runif(n, min = A, max = B)  #uniform distribution between 0 and 1   std dev =sqrt((B-A)^2/12)
    # factor=5 amount=0  jitter is runif(n, -amount, amount)      amount =0  <- factor * z/50 (same as S).  z <- max(x) - min(x)
    # 1/10 (max-min)   std=(max-min) sqrt(1/10*delta^2/12)   delta*sqrt(1/120)=0.09  ~10%
    d_noise<-matrix(d_noise,nrow=nrow(d),ncol=ncol(d))
    rownames(d_noise)<-rownames(d); colnames(d_noise)<-colnames(d)
    d<-d_noise
    imagenan(d,main=paste0("Noise: rmult= ",rmult," cmult= ",cmult," Jitter=5"))
  }

  Td<-transformE(d, Ave=TRUE,diagonal=diagonal)   #best?     most tests Sat night Nov 26

  runstats(Td)
  return(Td)
}
