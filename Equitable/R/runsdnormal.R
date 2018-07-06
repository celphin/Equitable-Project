runsdnormal <-
function(start, end, cmult,fac,
                      actualFunction,Ave=TRUE,diagonal=TRUE,pf=FALSE,ipf=FALSE,C=1.028){
  sdx<-sapply(start:end, FUN=function(rmult,cmult,fac,eg,Ave,diagonal,ipf,pf,C){

    Td<-Tnorm(rmult,cmult,fac,noise=FALSE, FUN=eg,Ave=Ave,diagonal=diagonal)
    Td_noise<-Tnorm(rmult,cmult,fac, FUN=eg,Ave=Ave,diagonal=diagonal)
    runstatsNS(Td,Td_noise)
    sdE<-sd(Td_noise$ET.x-Td$ET.x, na.rm = TRUE)
    sdls<-sd(Td_noise$l.s.x-Td$ET.x, na.rm = TRUE)
    sdEave<-sd(Td_noise$Ave.ET.x-Td$ET.x, na.rm = TRUE)
    sdnoise<-sd(Td_noise$smat-Td$ET.x, na.rm = TRUE)
    fsqrt<-sqrt(1/nrow(Td$smat)+1/ncol(Td$smat))
    sdsqrt<-fsqrt*sdnoise
    sigorig<-sd(Td_noise$smat, na.rm = TRUE)

    lm.ls_vs_sig <- lm(c(Td_noise$l.s.x) ~ c(Td$smat), na.action=na.exclude)
    b<-coef(lm.ls_vs_sig)[1]; a<-coef(lm.ls_vs_sig)[2]
    cat("\nLeast squared vs signal linear fit \n")
    #print(summary(lm.ls_vs_sig))
    #a<-(0.887)^2  ; b<- 0.54 ;storig<-2.42; sig0<-0.67;

    ms<-mean(Td$smat, na.rm=TRUE)
    sdcalc<-sqrt((a-1)^2*sigorig^2+(a^2)*(sdsqrt)^2+b^2  +(a-1)*b*ms)

    stdvalues<-stats_residuals(Td_noise,Td=Td,ipf=ipf,pf=pf,C=C)

    sdall<-list(rmult=rmult,cmult=cmult,nr=nrow(Td$smat),nc=ncol(Td$smat),sdnoise=sdnoise,
                theory_sdN_Ave_plus  =stdvalues$theory_sdN_Ave_plus, sdnoise_approx = stdvalues$sdnoise_approx ,theory_sdN_Ave=stdvalues$theory_sdN_Ave,
                sdE= sdE,sdls=sdls,sdEave=sdEave,fsqrt= fsqrt,sdsqrt=sdsqrt,sdcalc=sdcalc,sdN_Ave=stdvalues$sdN_Ave)
    return(sdall)
  },cmult=cmult,fac=fac, eg=actualFunction,simplify = TRUE,Ave=Ave,diagonal=diagonal,ipf=ipf,pf=pf,C=C )
  sdx<-matrix(as.numeric(sdx), nrow=nrow(sdx),ncol=ncol(sdx),
              dimnames=list(rownames(sdx),colnames(sdx)))
  plot(sdx["nr",], sdx["sdnoise",],ylim=c(0,max(sdx["sdnoise",])),ylab="Standard Deviation",xlab="Number of Rows",
       main=paste0("Standard deviation vs Number of Rows \n",
                   " cmult= ", cmult," std fac=",fac," C=",C))
  pch<-t((c(1,15,11,NA,NA,NA,NA,NA)))
  legend<-c('Noise:I-S','T-S','L.S.-S','Row/Col Ave Noise',"Predicted:L.S.(Bias)", "Predicted Noise","Predicted:T-S","True Noise Ave" )
  lty<-c(NA,NA,NA,1,3,   4,5,3)
  lwd<-c(NA,NA,NA,4,2,   3,2,4)
  legend('bottomleft',inset=.00,legend=legend,
         lwd=lwd,lty=lty,pch = pch,bg='white',ncol=c(2),cex=0.75)

  #lines(sdx["nr",], sdx["sdsqrt",],lty=1,lwd=4)
  lines(sdx["nr",], sdx["theory_sdN_Ave",],lty=1,lwd=4)
  lines(sdx["nr",], sdx["sdls",],type="p",pch=11)
  #lines(sdx["nr",], sdx["fsqrt",],lty=1,lwd=4)
  lines(sdx["nr",], sdx["sdE",],type="p",pch=15)
  lines(sdx["nr",], sdx["sdcalc",],lty=3,lwd=2)
  lines(sdx["nr",], sdx["sdnoise_approx",],lty=4,lwd=4)
  lines(sdx["nr",], sdx["theory_sdN_Ave_plus",],lty=5,lwd=2)
  lines(sdx["nr",], sdx["sdN_Ave",],lty=3,lwd=4)
  #lines(sdx["nr",], sdx["sdEave",],type="p",pch=24)
  return(sdx)
}
