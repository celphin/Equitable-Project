plotp1vsp2 <-
function(rmean,rmeano,y,yo,stdy,ylim=NULL,ps=1,r2=0,
                     s=NULL,b=NULL,se=NULL,be=NULL,sN=NULL,bN=NULL,refname="unknown",cname="unknown",br=""){
 # lotscol<-colors()[c(24,94,26,130,121,96,97,49,47,417,256,8,33,90,142,144,653)]
  lotscol<-colors()[c(24,94,26,124,633,450,453,11,68,254,257,51,630,76,142,150,653)] #plot(1:length(lotscol),col=lotscol,pch=15,cex=4)
  lotscol<-c(lotscol,"darkorchid","darkkhaki","lightpink" ,"lightskyblue")
  if(length(unique(names(y)))<=length(lotscol) && length(unique(names(y)))>1){
    colourevent<-lotscol[ 1:length(unique(names(y)))]
    if(length(unique(names(yo)))<=8)colourevent<-c((1:length(y))+9)    # co<-1:length(unique(rownames(y)));
    if(length(unique(names(y)))!=length(y)){
    colourevent<-rep(NA,length(y))
    for(j in 1:length(unique(names(y)))){ colourevent[which(names(y)==unique(names(y))[j])]<-j  }
    }
  } else colourevent<-"black"
  plotx_vs_y_with_errors(x=rmean,y=y,stdy=stdy,ylim=ylim,
                         xlab=paste0("Ref. ",refname),ylab=paste0("Non-Ref. ",cname),
                         main=paste0(br,"\nNon-ref. ",cname,"\nVs Ref. ",refname),cex.main=0.7)
  points(rmeano,yo,
         pch=19,cex=1.5, col=colourevent)
  lines(rmean,y,lty=1)

  if(length(unique(names(y)))<25 && length(unique(names(y)))>1){

    if(length(unique(names(y)))!=length(y)){
      legend("bottomright",inset= 0.0,(unique(names(y))), fill=colourevent )
    } else legend("bottomright",inset= 0.0,(names(y)), fill=colourevent )
  }

  legend<-c("Equitable","Slope of 1","Original")
  lwd<-c(1,1,NA)
  lty<-c(1,2,NA)
  pch<-c(15,NA,19)

  legend('topleft',inset=.02,legend=legend,
         lwd=lwd,lty=lty,pch = pch,bg='white',ncol=c(2),cex=0.75)

  legend("bottomleft", legend = c(paste0("p= ",round(ps,digits=5) ),paste0("Coef.Det.= ",round(r2,digits=3) ) ))
  if(!is.null(s)&&!is.null(b) ){
    if(!is.null(se)&&!is.null(be) ) legend("bottom",
                                       legend = c(paste0("Slope= ",round(s,digits=2) ),paste0("(95% ",round(se,digits=2),") N=",sN ),
                                       paste0("Inter.= ",round(b,digits=1)),paste0("(95% ",round(be,digits=1),") N=",bN ) )) else
    legend("bottom", legend = c(paste0("Slope= ",round(s,digits=2) ),paste0("Intercept.= ",round(b,digits=1) ) ))
  }
}
