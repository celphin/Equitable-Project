changeNAnormal <-
function(rmult, cmult,start, end, inc,fac,
                            actualFunction,Ave=TRUE,diagonal=TRUE,pf=FALSE,ipf=FALSE,C=1.028){
  sdx<-sapply(seq(start,end, by=inc), FUN=function(rmult,cmult,fac,eg,Ave,diagonal,NAfrac,ipf,pf,C){
    cat("\n proportion of NA values is",NAfrac,"\n")
    T4<-TnormNA(rmult,cmult,fac=fac, FUN=eg,Ave=Ave,diagonal=diagonal,NAfrac=NAfrac,imageplot=TRUE)
    Td<-T4$Td
    Td_noise<-T4$TdnNA
    imagenan(Td_noise$ET.x,main=paste0("Equitable Transform from noisy data\n with ",NAfrac," missing data"))
    runstatsNS(Td,Td_noise)
    sdE<-sd(Td_noise$ET.x-Td$ET.x, na.rm = TRUE)
    sdls<-sd(Td_noise$l.s.x-Td$ET.x, na.rm = TRUE)
    sdEave<-sd(Td_noise$Ave.ET.x-Td$ET.x, na.rm = TRUE)
    sdnoise<-sd(Td_noise$smat-Td$ET.x, na.rm = TRUE)
    fsqrt<-sqrt(1/nrow(Td$smat)+1/ncol(Td$smat))

    sumNA<-sum(length(which(is.na(Td_noise$smat))))

    Nt<-prod(dim(Td_noise$smat))
    fr<-sumNA/Nt
    # cat("\ntotal of NA transformed data is ",sumNA," with total ",Nt," Fraction of data set that is NA is ",fr )
    fac<-sqrt(1-fr)
    newM<-nrow(Td_noise$smat)*fac
    newN<-ncol(Td_noise$smat)*fac
    sdfactornew<- sqrt(1/(newN-1)+1/(newM-1))
    sdfactor<- sqrt(1/(nrow(Td_noise$smat)-1)+1/(ncol(Td_noise$smat)-1))
    # cat("\n  col= ",ncol(Td_noise$smat),"  row= ",nrow(T_noise$smat), " initial scale factor= ",sdfactor)
    # cat("\neffective  col= ",newN,"effective  row= ",newM, " scale factor= ",sdfactornew, "final = ",sdnoise*sdfactornew)
    sdsqrtnew<-sdnoise*sdfactornew
    sdsqrt<-sdfactor*sdnoise

    sigorig<-sd(Td_noise$smat, na.rm = TRUE)

    lm.ls_vs_sig <- lm(c(Td_noise$l.s.x) ~ c(Td$smat), na.action=na.exclude)
    b<-coef(lm.ls_vs_sig)[1]; a<-coef(lm.ls_vs_sig)[2]
    cat("\nLeast squared vs signal linear fit \n")
    #print(summary(lm.ls_vs_sig))
    #a<-(0.887)^2  ; b<- 0.54 ;storig<-2.42; sig0<-0.67;
    ms<-mean(Td$smat, na.rm=TRUE)
    sdcalc<-sqrt((a-1)^2*sigorig^2+(a^2)*(sdsqrt)^2+b^2  +(a-1)*b*ms)
    stdvalues<-stats_residuals(Td_noise,Td=Td,ipf=ipf,pf=pf,C=C)

    sdall<-list(NAfrac=NAfrac,fac=fac,rmult=rmult,cmult=cmult,nr=nrow(Td$smat),nc=ncol(Td$smat),sdnoise=sdnoise,
                theory_sdN_Ave_plus  =stdvalues$theory_sdN_Ave_plus, sdnoise_approx = stdvalues$sdnoise_approx ,theory_sdN_Ave=stdvalues$theory_sdN_Ave,
                sdE= sdE,sdls=sdls,sdEave=sdEave,fsqrt= fsqrt,sdsqrt=sdsqrt,sdsqrtnew=sdsqrtnew,sdcalc=sdcalc)

    return(sdall)
  },rmult=rmult,cmult=cmult,fac=fac, eg=actualFunction,simplify = TRUE ,Ave=Ave,diagonal=diagonal,ipf=ipf,pf=pf,C=C)
  sdx<-matrix(as.numeric(sdx), nrow=nrow(sdx),ncol=ncol(sdx),
              dimnames=list(rownames(sdx),colnames(sdx)))

  # plot(sdx["NAfrac",], sdx["sdnoise",],ylim=c(0,max(sdx["sdnoise",],sdx["sdE",])),
  #      main=paste0("squares=Equitable,ls=Stars,noise=circles\nNormal: line= lowest std dev \n",
  #                  "cmult= ", cmult," rmult=",rmult," fac=",fac," C=",C ))
  # lines(sdx["NAfrac",], sdx["sdsqrt",])
  # #lines(sdx["NAfrac",], sdx["sdsqrtnew",])
  # lines(sdx["NAfrac",], sdx["sdls",],type="p",pch=11)
  # #lines(sdx["NAfrac",], sdx["fsqrt",])
  # lines(sdx["NAfrac",], sdx["sdE",],type="p",pch=15)
  # #lines(sdx["NAfrac",], sdx["sdcalc",])
  # #lines(sdx["NAfrac",], sdx["sdEave",],type="p",pch="O")
  #
  plot(sdx["NAfrac",], sdx["sdnoise",],ylim=c(0,max(sdx["sdnoise",])),ylab="Standard Deviation",xlab="Missing Fraction of Original Signal",
       main=paste0("Standard Deviation vs Missing Fraction of Original Signal \n",
                   "cmult= ", cmult," rmult",rmult))
  # pch<-t((c(1,15,11,NA,NA)))
  # legend<-c('Original','Equitable','Least Squared','Minimum',"Least Squares (Bias)")
  # lty<-c(NA,NA,NA,1,2)
  # lwd<-c(NA,NA,NA,4,2)
  # legend('topleft',inset=.02,legend=legend,
  #        lwd=lwd,lty=lty,pch = pch,bg='white',ncol=c(2),cex=0.75)

  pch<-t((c(1,15,11,NA,NA,NA,NA)))
  legend<-c('Noise:I-S','T-S','L.S.-S','Row/Col Ave Noise',"Predicted:L.S.(Bias)", "Predicted Noise","Predicted:T-S" )
  lty<-c(NA,NA,NA,1,3,   4,5)
  lwd<-c(NA,NA,NA,4,2,   3,2)
   legend('bottomleft',inset=.00,legend=legend,
         lwd=lwd,lty=lty,pch = pch,bg='white',ncol=c(2),cex=0.75)
  #lines(sdx["NAfrac",], sdx["sdsqrt",],lty=1,lwd=4)
  lines(sdx["NAfrac",], sdx["theory_sdN_Ave",],lty=1,lwd=4)
  lines(sdx["NAfrac",], sdx["sdls",],type="p",pch=11)
  #lines(sdx["NAfrac",], sdx["fsqrt",],lty=1,lwd=4)
  lines(sdx["NAfrac",], sdx["sdE",],type="p",pch=15)
  lines(sdx["NAfrac",], sdx["sdcalc",],lty=3,lwd=2)
  lines(sdx["NAfrac",], sdx["sdnoise_approx",],lty=4,lwd=4)
  lines(sdx["NAfrac",], sdx["theory_sdN_Ave_plus",],lty=5,lwd=2)
  #lines(sdx["NAfrac",], sdx["sdEave",],type="p",pch=24)

  # lines(sdx["NAfrac",], sdx["sdsqrt",],lty=1,lwd=4)
  # lines(sdx["NAfrac",], sdx["sdls",],type="p",pch=11)
  # #lines(sdx["NAfrac",], sdx["fsqrt",],lty=1,lwd=4)
  # lines(sdx["NAfrac",], sdx["sdE",],type="p",pch=15)
  # lines(sdx["NAfrac",], sdx["sdcalc",],lty=2,lwd=2)
  # #lines(sdx["NAfrac",], sdx["sdEave",],type="p",pch=24)
  return(sdx)
}
