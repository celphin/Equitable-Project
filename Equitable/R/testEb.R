testEb <-
function(b,s,minval=1e-6){
      xm<-mean(abs(c(b)), na.rm = TRUE)


      if(abs(xm)>minval) rtest<-(b+s*t(b))/xm else rtest<-0*b
      xm<-mean(c(rtest), na.rm = TRUE)
      xsd<-sd(c(rtest), na.rm = TRUE)
      t<-list(xm=xm, xsd=xsd)
      return(t)
    }
