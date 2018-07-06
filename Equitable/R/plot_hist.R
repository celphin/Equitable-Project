plot_hist <-
function(Tdave,refer=NULL,main=" ",slim=NULL,blim=NULL){

  if(!is.null(refer)){

    cat("\n SLOPE summary for best" ,main," \n")
    si<-summary(c(Tdave$E.s[,refer]),na.rm=TRUE)
    print(si)
    #if(is.null(slim))slim<-c(si["Median"]-3*abs(si["Median"]-si["1st Qu."]),si["Median"]+3*abs(si["Median"]-si["3rd Qu."]))

    if(is.null(slim)){slim<-c(mean(c(Tdave$E.s),na.rm = TRUE)-2*sd(c(Tdave$E.s),na.rm = TRUE),
                              mean(c(Tdave$E.s),na.rm = TRUE)+2*sd(c(Tdave$E.s),na.rm = TRUE))}
    hist(Tdave$E.s[,refer],breaks=30,xlim=slim,main="")
    title(main=paste("\nEquitable Matrix SLOPES for",main ),cex.main=0.7)
    lines(rep(si["Median"],1000),0:999,lwd=4)

    lines(rep(si["1st Qu."],1000),0:999,lty=2,lwd=2)
    lines(rep(si["3rd Qu."],1000),0:999,lty=3,lwd=2)
    lines(rep(1,1001),seq(0,0.5,by=(0.5/1000)),lty=1,lwd=8)

    legend<-c(paste("Median",round(si["Median"],digits=2)),paste("1st Qu.",round(si["1st Qu."],digits=2)),
              paste("3rd Qu.",round(si["3rd Qu."],digits=2)),"Slope=1")
    lwd<-c(4,2,2,8)
    lty<-c(1,2,3,1)
    legend('topleft',inset=.02,legend=legend,
           lwd=lwd,lty=lty,bg='white',cex=0.75)



    cat("\n INTERCEPT summary for best" ,main," \n")
    bi<-summary(c(Tdave$E.b[,refer]),na.rm=TRUE)
    print(bi)
    #if(is.null(blim))blim<-c(bi["Median"]-3*abs(bi["Median"]-si["1st Qu."]),bi["Median"]+3*abs(bi["Median"]-bi["3rd Qu."]))
    if(is.null(blim)){blim<-c(mean(c(Tdave$E.b),na.rm = TRUE)-4*sd(c(Tdave$E.b),na.rm = TRUE),
                              mean(c(Tdave$E.b),na.rm = TRUE)+4*sd(c(Tdave$E.b),na.rm = TRUE))}
    if(length(which(!is.na(Tdave$E.b[,refer])))>0){
      hist(Tdave$E.b[,refer],breaks=50,xlim=blim,main="")
      title(main=paste("\nEquitable Matrix INTERCEPTS for",main ),cex.main=0.7)
      lines(rep(bi["Median"],1000),0:999,lwd=4)

      lines(rep(bi["1st Qu."],1000),0:999,lty=2,lwd=2)
      lines(rep(bi["3rd Qu."],1000),0:999,lty=3,lwd=2)
      lines(rep(0,1001),seq(0,0.5,by=(0.5/1000)),lty=4,lwd=8,type="p",pch=19)
      legend<-c(paste("Median",round(bi["Median"],digits=0)),paste("1st Qu.",round(bi["1st Qu."],digits=0)),
                paste("3rd Qu.",round(bi["3rd Qu."],digits=0)),"Intercept=0")
      lwd<-c(4,2,2,8)
      lty<-c(1,2,3,1)
      legend('topleft',inset=.02,legend=legend,
             lwd=lwd,lty=lty,bg='white',cex=0.75)
    } else {cat("\n Intercept column is completely NAs\n")}
  } else {

    cat("\n SLOPE summary  \n")
    si<-summary(c(Tdave$E.s),na.rm=TRUE)
    print(si)
    #if(is.null(slim))slim<-c(si["Median"]-3*abs(si["Median"]-si["1st Qu."]),si["Median"]+3*abs(si["Median"]-si["3rd Qu."]))
    if(is.null(slim)){
      #slim<-c(min(c(Tdave$E.s),na.rm=TRUE)-3*sd(c(Tdave$E.s),na.rm=TRUE),max(c(Tdave$E.s)+3*sd(c(Tdave$E.s),na.rm=TRUE),na.rm=TRUE))
      slim<- c(mean(c(Tdave$E.s),na.rm = TRUE)-2*sd(c(Tdave$E.s),na.rm = TRUE),
               mean(c(Tdave$E.s),na.rm = TRUE)+2*sd(c(Tdave$E.s),na.rm = TRUE))
      hist(Tdave$E.s,xlim=slim,main="")    #
    } else {
      hist(Tdave$E.s,breaks=30,xlim=slim,main="")
    }
    title(main=paste("\nEquitable Matrix SLOPES for",main ),cex.main=0.7)
    lines(rep(si["Median"],1000),0:999,lwd=4)

    lines(rep(si["1st Qu."],1000),0:999,lty=2,lwd=2)
    lines(rep(si["3rd Qu."],1000),0:999,lty=3,lwd=2)
    lines(rep(1,1001),seq(0,0.5,by=(0.5/1000)),lty=1,lwd=8,type="p",pch=19)
    legend<-c(paste("Median",round(si["Median"],digits=2)),paste("1st Qu.",round(si["1st Qu."],digits=2)),
              paste("3rd Qu.",round(si["3rd Qu."],digits=2)),"Slope=1")
    lwd<-c(4,2,2,8)
    lty<-c(1,2,3,1)
    legend('topleft',inset=.02,legend=legend,
           lwd=lwd,lty=lty,bg='white',cex=0.75)


    cat("\n SLOPE summary (Exclude slopes that cannot be distinguished from 1) \n")
    nsig=2
    sss<-Tdave$E.s
    sse<-nsig*Tdave$E.sd1/sqrt(Tdave$E.sN)
    sss[abs(sss-1)-sse<0]<-NA
    si<-summary(c(sss),na.rm=TRUE)
    print(si)
    #if(is.null(slim))slim<-c(si["Median"]-3*abs(si["Median"]-si["1st Qu."]),si["Median"]+3*abs(si["Median"]-si["3rd Qu."]))
    if(is.null(slim)){c(mean(c(Tdave$E.s),na.rm = TRUE)-2*sd(c(Tdave$E.s),na.rm = TRUE),
                        mean(c(Tdave$E.s),na.rm = TRUE)+2*sd(c(Tdave$E.s),na.rm = TRUE))}
    hist(sss,breaks=30,xlim=slim,main="")
    title(main=paste("\nSLOPES (Exclude slopes not different from1)",main ),cex.main=0.7)
    lines(rep(1.1,1000),0:999,lty=1,lwd=3)
    lines(rep(1/1.1,1000),0:999,lty=2,lwd=2)
    lines(rep(1,1001),seq(0,0.5,by=(0.5/1000)),lty=1,lwd=8,type="p",pch=19)
    legend<-c(paste("Slope=1.1"),paste("Slope=",round(1/1.1,digits=2))
              ,"Slope=1")
    lwd<-c(3,2,8)
    lty<-c(1,2,1)
    legend('topleft',inset=.02,legend=legend,
           lwd=lwd,lty=lty,bg='white',cex=0.75)


    cat("\n INTERCEPT summary  \n")
    bi<-summary(c(Tdave$E.b),na.rm=TRUE)
    print(bi)
    #if(is.null(blim))blim<-c(bi["Median"]-3*abs(bi["Median"]-bi["1st Qu."]),bi["Median"]+3*abs(bi["Median"]-bi["3rd Qu."]))
    if(is.null(blim)){
      #blim<-c(min(c(Tdave$E.b),na.rm=TRUE)-3*sd(c(Tdave$E.b),na.rm=TRUE),max(c(Tdave$E.b)+3*sd(c(Tdave$E.b),na.rm=TRUE),na.rm=TRUE))
      blim<- c(mean(c(Tdave$E.b),na.rm = TRUE)-4*sd(c(Tdave$E.b),na.rm = TRUE),mean(c(Tdave$E.b),na.rm = TRUE)+4*sd(c(Tdave$E.b),na.rm = TRUE))
      hist(Tdave$E.b,xlim=blim,main="")
    } else {
      hist(Tdave$E.b,breaks=50,xlim=blim,main="")
    }

    title(main=paste("\nEquitable Matrix INTERCEPTS for",main ),cex.main=0.7)
    lines(rep(bi["Median"],1000),0:999,lwd=4)

    lines(rep(bi["1st Qu."],1000),0:999,lty=2,lwd=2)
    lines(rep(bi["3rd Qu."],1000),0:999,lty=3,lwd=2)
    lines(rep(0,1001),seq(0,0.5,by=(0.5/1000)),lty=4,lwd=8,type="p",pch=19)
    legend<-c(paste("Median",round(bi["Median"],digits=0)),paste("1st Qu.",round(bi["1st Qu."],digits=0)),
              paste("3rd Qu.",round(bi["3rd Qu."],digits=0)),"Intercept=0")
    lwd<-c(4,2,2,8)
    lty<-c(1,2,3,1)
    legend('topleft',inset=.02,legend=legend,
           lwd=lwd,lty=lty,bg='white',cex=0.75)
  }
}
