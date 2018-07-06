runsd <-
function(start, end, cmult,
                actualFunction){
  sdx<-sapply(start:end, FUN=function(rmult,cmult,eg,diagonal){

    Td<-T(rmult,cmult,noise=FALSE, FUN=eg,diagonal=diagonal)
    Td_noise<-T(rmult,cmult, FUN=eg,diagonal=diagonal)
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

    sdall<-list(rmult=rmult,cmult=cmult,nr=nrow(Td$smat),nc=ncol(Td$smat),sdnoise=sdnoise,
                sdE= sdE,sdls=sdls,sdEave=sdEave,fsqrt= fsqrt,sdsqrt=sdsqrt,sdcalc=sdcalc)
    return(sdall)
  },cmult=cmult, eg=actualFunction,simplify = TRUE,diagonal=TRUE )
  sdx<-matrix(as.numeric(sdx), nrow=nrow(sdx),ncol=ncol(sdx),
              dimnames=list(rownames(sdx),colnames(sdx)))
  plot(sdx["nr",], sdx["sdnoise",],ylim=c(0,max(sdx["sdnoise",])),
       main=paste0("squares=Equitable,ls=Stars,noise=circles\nJitter: line= lowest std dev \n",
                   "cmult= ", cmult))
  lines(sdx["nr",], sdx["sdsqrt",])
  lines(sdx["nr",], sdx["sdls",],type="p",pch=11)
  #lines(sdx["nr",], sdx["fsqrt",])
  lines(sdx["nr",], sdx["sdE",],type="p",pch=15)
  lines(sdx["nr",], sdx["sdcalc",])
  #lines(sdx["nr",], sdx["sdEave",],type="p",pch="O")
  cat("\n",sdx)
}
