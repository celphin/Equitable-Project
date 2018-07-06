testE <-
function(s){      #test for equitable using property axy*ayx=1 if equitable
      rtest<-s*t(s)
      xm<-mean(c(rtest), na.rm = TRUE)
      xsd<-sd(c(rtest), na.rm = TRUE)
      t<-list(xm=xm, xsd=xsd)
      return(t)
    }
