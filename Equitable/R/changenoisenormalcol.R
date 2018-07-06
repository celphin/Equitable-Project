changenoisenormalcol <-
function(rmult, cmult, start, end, inc,
                             actualFunction){
  sdx<-sapply(seq(start,end, by=inc), FUN=function(rmult,cmult,fac,eg,diagonal){
    cat("\n factor for noise is",fac,"\n")
    Td<-Tnormcol(rmult,cmult,fac,noise=FALSE, FUN=eg,diagonal=diagonal)
    Td_noise<-Tnormcol(rmult,cmult,fac, FUN=eg,diagonal=diagonal)
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


    sdall<-list(fac=fac,rmult=rmult,cmult=cmult,nr=nrow(Td$smat),nc=ncol(Td$smat),sdnoise=sdnoise,
                sdE= sdE,sdls=sdls,sdEave=sdEave,fsqrt= fsqrt,sdsqrt=sdsqrt,sdcalc=sdcalc)
    return(sdall)
  },rmult=rmult,cmult=cmult, eg=actualFunction,simplify = TRUE,diagonal=TRUE )
  sdx<-matrix(as.numeric(sdx), nrow=nrow(sdx),ncol=ncol(sdx),
              dimnames=list(rownames(sdx),colnames(sdx)))
  plot(sdx["fac",], sdx["sdnoise",],ylim=c(0,max(sdx["sdnoise",])),
       main=paste0(" squares=Equitable,ls=Stars,noise=circles\nNorm col: line= lowest std dev \n",
                   "cmult= ", cmult," rmult=",rmult))
  lines(sdx["fac",], sdx["sdsqrt",])
  lines(sdx["fac",], sdx["sdls",],type="p",pch=11)
  #lines(sdx["fac",], sdx["fsqrt",])
  lines(sdx["fac",], sdx["sdE",],type="p",pch=15)
  lines(sdx["fac",], sdx["sdcalc",])
  lines(sdx["fac",], sdx["sdEave",],type="p",pch="O")

  return(sdx)
}
