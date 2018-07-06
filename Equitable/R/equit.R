equit <-
function(E,p, r2max=0.001, semax=0.01, maxrun=8){
      nc<-ncol( E$s)
     cat("Slope: iteration ", E$numrun," Average R^2 test ", E$rtestxm," Average std dev of R^2 ",E$rtestxsd," \n")
     if(is.na(E$rtestxsd)) {E$rtestxsd<-0; cat("\nRtest std reset to 0")}
    if((abs(E$rtestxm-1)< r2max && E$rtestxsd< semax) || E$numrun>=maxrun){
    return(E)
    } else{     #if not good enough redo with new matrix Es
      E$numrun<-E$numrun+1    #run again

      Es<-outer(1:nc,1:nc,Vectorize(Eslope),
                s=as.data.frame(c(E$s)),nc=nc,p=as.data.frame(c(p)))  #input matrices must be as data.frames output must be same length
      colnames(Es)<-colnames(E$s) ; rownames(Es)<-rownames(E$s); E$s<-Es
      rtest<-testE(Es)
      E$rtestxm<-rtest$xm; E$rtestxsd<-rtest$xsd; E$s<-Es
      E<-equit(E,p)

                 #test new matrix for equitability  rtest$xm=mean r^2 and rtest$xsd =std dev
    }

    }
