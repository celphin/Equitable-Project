plot1vs2 <-
function(rmeano,yo,ylim=NULL,
                     refname="unknown",cname="unknown",br=""){
  if(is.null(ylim)){
    mins<-min(c(yo,rmeano),na.rm = TRUE)
    maxs<-max(c(yo,rmeano),na.rm = TRUE)
    ds<-maxs-mins
    if(length(yo)<=21) ylim<-c(mins-1*ds*3/8,maxs+ds*3/8) else ylim<-c(mins-1*ds*3/8,maxs+ds*0/8)

    if((ylim[2]-ylim[1])<1e-10)ylim<-c(0,1)
  }
  # lotscol<-colors()[c(24,94,26,130,121,96,97,49,47,417,256,8,33,90,142,144,653)]
  lotscol<-colors()[c(24,94,26,124,633,450,453,11,68,254,257,51,630,76,142,150,653)] #plot(1:length(lotscol),col=lotscol,pch=15,cex=4)
  lotscol<-c(lotscol,"darkorchid","darkkhaki","lightpink" ,"lightskyblue")

  if(length(unique(names(yo)))<=length(lotscol) && length(unique(names(yo)))>1){
     colourevent<-lotscol[ 1:length(unique(names(yo)))]
     if(length(unique(names(yo)))<=8) colourevent<-c((1:length(yo))+9)    # co<-1:length(unique(rownames(yo)));
     if(length(unique(names(yo)))!=length(yo)){
      colourevent<-rep(NA,length(yo))
      for(j in 1:length(unique(names(yo)))){ colourevent[which(names(yo)==unique(names(yo))[j])]<-j  }
    }
  } else colourevent<-"black"
  plot(rmeano,yo, pch=19,cex=2, col=colourevent,ylim=ylim,xlim=ylim,
                         xlab=paste0("Ref. ",refname),ylab=paste0("Non-Ref. ",cname),
                         main=paste0(br,"\nNon-ref. ",cname,"\nVs Ref. ",refname),cex.main=1)
  lines(rmeano,rmeano,lty=1)
  # points(rmeano,yo,
  #        pch=19,cex=1.5, col=colourevent)
  # lines(rmean,yo,lty=1)

  if(length(unique(names(yo)))<25 && length(unique(names(yo)))>1){

    if(length(unique(names(yo)))!=length(yo)){
      legend("bottomright",inset= 0.0,(unique(names(yo))), fill=colourevent )
    } else legend("bottomright",inset= 0.0,(names(yo)), fill=colourevent )
  }

  }
