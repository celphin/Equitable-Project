equitable <-
function(mat){
    cat("\n Average 1-p : ",mean(1-mat$pslope,na.rm=TRUE), "\n")  #mean(1-Td96$l.s.pslope,na.rm=TRUE) mean(1-c(Td96$l.s.pslope),na.rm=TRUE)
    nc<-ncol(mat$s)
    s1<-mat$s
    rtest<-testE(s1)        #test corrected matrix for equitability
    cat("Equitable: Average R^2 test ", rtest$xm," Average std dev of R^2 ",rtest$xsd," \n")
    s1andnode <-reduce_s(s1,mat$sse,mat$pslope,mat$s,mat$sse,mat$node)
      s1<-s1andnode$s1
    node<-s1andnode$node
    mat$b<-s1andnode$b1

    rtest<-testE(s1)        #test corrected matrix for equitability
    cat("Equitable: Before iteration (Reduced slopes) Average R^2 test ", rtest$xm," Average std dev of R^2 ",rtest$xsd," \n\n")
    #first run calc sd for each slope as well
    Es<-outer(1:nc,1:nc,Vectorize(Eslope),
              s=as.data.frame(c(s1)),nc=nc,p=as.data.frame(c(mat$pslope)))  #input matrices must be as data.frames output must be same length
    colnames(Es)<-colnames(mat$s) ; rownames(Es)<-rownames(mat$s)
    E1s<-Es
    E1sd<-outer(1:nc,1:nc,Vectorize(Esd),
               s=as.data.frame(c(s1)),nc=nc,p=as.data.frame(c(mat$pslope)))  # first iteration find errors for each slope
    colnames(E1sd)<-colnames(mat$s) ; rownames(E1sd)<-rownames(mat$s)
    E1sN<-outer(1:nc,1:nc,Vectorize(sN),
                s=as.data.frame(c(s1)),nc=nc,p=as.data.frame(c(mat$pslope)))  # first iteration find errors for each slope
    colnames(E1sN)<-colnames(mat$s) ; rownames(E1sN)<-rownames(mat$s)

    rtest<-testE(Es)
   numrun=1
   # r2max=0.001, semax=0.01, maxrun=8
    E<-list(
      s=Es,
      numrun=numrun,
      rtestxm=rtest$xm ,     #xm and xsd
      rtestxsd=rtest$xsd,
      s1=E1s,     #first iteration matrix
      sd1=E1sd,     #first iteration matrix sd dev error for each slope
      sN=E1sN,
      snode=node

    )
    E<-equit(E,mat$pslope)  # E is list and Es is latest slope matrix

    #after finishing put por slopes back in
    #imagenan(E$s,zlim=c(0,4),main=" Equitable:Pre pre Final",xlab="Row", ylab="Col")
    E$s[which(is.na(E$s))]<-mat$s[which(is.na(E$s))]

    E$s1[which(is.na(E$s1))]<-mat$s[which(is.na(E$s1))]
    E$sd1[which(is.na(E$s1))]<-mat$sse[which(is.na(E$s1))]
    #imagenan(E$s,zlim=c(0,4),main=" Equitable:Pre Final",xlab="Row", ylab="Col")
    #s1andnode<-reduce_s(E$s,E$sd1,mat$pslope,mat$s,mat$sse,mat$node,minp,mins,sdfactor,asigfactor,b=TRUE)   #rerun setting some slopes to 1e-10 and their transpose to NA
    er<-matrix(0,nrow=nrow(E$s),ncol=ncol(E$s))
    s1andnode<-reduce_s(E$s,mat$sse,mat$pslope,mat$s,mat$sse,mat$node,b=TRUE)   #rerun setting some slopes to 1e-10 and
    E$s<-s1andnode$s1
    E$snode<-s1andnode$node

    #imagenan(E$s,zlim=c(0,4),main=" Equitable:Final",xlab="Row", ylab="Col")

    return(E)
  }
