equitb <-
function(E,Eb,s1,p, maxzero_std=0.1, zero=0.001, maxrun=3){   #numrun from 8 down to 3 &std from 0.015 to 0.1 (Jan 12 2018)
       nc<-ncol( E$b)
       cat("Intercept: ",E$numrun,E$rtestbm,E$rtestbsd,"\n")
      if ((abs(E$rtestbm) < zero  && E$rtestbsd < maxzero_std ) ||  E$numrun >=  maxrun) {  #return condition met
         return(E)
       } else{     #if not good enough redo with new matrix Es
         E$numrun<-E$numrun+1    #run again
         Eb<-outer(1:nc,1:nc,Vectorize(Eintercept),b=as.data.frame(c(Eb)),
                   s=as.data.frame(c(s1)),nc=nc,p=as.data.frame(c(p)))  #input matrices must be as data.frames output must be same length

         colnames(Eb)<-colnames(s1) ; rownames(Eb)<-rownames(s1)
         rtest<-testEb(Eb,s1)
         E$rtestbm<-rtest$xm; E$rtestbsd<-rtest$xsd ; E$b<-Eb

         E<-equitb(E,Eb,s1,p)

         #test new matrix for equitability  rtest$xm=mean r^2 and rtest$xsd =std dev
       }

     }
