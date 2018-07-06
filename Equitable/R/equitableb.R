equitableb <-
function(mat){     #was 10e-6 and 1 better at 10e-2 and 2  try 10e-1 and 2 try 1/8 and 1

  nc<-ncol(mat$b)
  #only use values whose ?? and slope prob<0.5
  s1<-mat$E.s    #this uses equitable values for slopes and reduce uses this slope-sse as check
  s1[is.na(t(s1))]<-NA
  b1<-mat$b
  b1[is.na(s1)]<-NA
 # imagenan(b1,zlim=c(-4,30),main=" Equitableb START: Intercept",xlab="Row", ylab="Col")

  rtest<-testEb(b1,s1)       #test corrected matrix for equitability
  cat("Equitableb: test values of least squares fits ",rtest$xm,rtest$xsd,"\n")

  #first run calc sd for each intercept as well
  Eb<-outer(1:nc,1:nc,Vectorize(Eintercept),b=as.data.frame(c(b1)),
            s=as.data.frame(c(s1)),nc=nc,p=as.data.frame(c(mat$pslope)))  #input matrices must be as data.frames output must be same length
  colnames(Eb)<-colnames(mat$b) ; rownames(Eb)<-rownames(mat$b)
  Eb1<-Eb
  Eb1sd<-outer(1:nc,1:nc,Vectorize(Esdintercept),b=as.data.frame(c(b1)),
               s=as.data.frame(c(s1)),nc=nc,p=as.data.frame(c(mat$pslope)))  # first iteration find errors for each slope
  colnames(Eb1sd)<-colnames(mat$s) ; rownames(Eb1sd)<-rownames(mat$s)
  Eb1N<-outer(1:nc,1:nc,Vectorize(bN),b=as.data.frame(c(b1)),
               s=as.data.frame(c(s1)),nc=nc,p=as.data.frame(c(mat$pslope)))  # first iteration find errors for each slope
  colnames(Eb1N)<-colnames(mat$s) ; rownames(Eb1N)<-rownames(mat$s)
  numrun=1
  rtest<-testEb(Eb,s1)
  #  maxrun=8 maxzero_std<-0.015, zero=0.001, maxrun=8
  bElist<-list(    #list of relevant intercept parameters
    b=Eb,
    numrun=numrun,
    rtestbm=rtest$xm ,     #xm and xsd
    rtestbsd=rtest$xsd,
    b1=Eb1,     #first iteration matrix
    bsd1=Eb1sd,     #first iteration matrix sd dev error for each intercept
    bN=Eb1N
  )

  bElist<-equitb(bElist,Eb,s1,mat$pslope)  # bElist is list and Eb is latest intercept matrix

  #after finishing put poor intercepts back in
  bElist$b[which(is.na(bElist$b))]<-mat$b[which(is.na(bElist$b))]
  bElist$b1[which(is.na(bElist$b1))]<-mat$b[which(is.na(bElist$b1))]
  bElist$bsd1[which(is.na(bElist$bsd1))]<-0
 # imagenan(bElist$b,zlim=c(-4,30),main=" Equitableb END: Intercept",xlab="Row", ylab="Col")
 # imagenan(bElist$b,zlim=c(0,4),main=" Equitableb: Final Intercept",xlab="Row", ylab="Col")
  return(bElist)
}
