Esd <-
function(y,x,s,nc,p){                                  # find approximation to equitable using vector of reference column slopes
   s<-matrix(s,nrow=nc,ncol=nc) ;p<-matrix(p,nrow=nc,ncol=nc)
   #print(s)
   sx_=s[x,]; sy_<- s[y,]; py_<-p[y,]; px_<-p[x,]
   j<-which(complete.cases(sx_,sy_))
   if(length(j)>0){
     xvect<-sy_[j]/sx_[j]
     wvect<-(1-py_[j])^2*(1-px_[j])^2
     xm<-weighted.mean(xvect, wvect, na.rm = TRUE)
     xsd<-sqrt(sum(wvect * (xvect - xm)^2,na.rm = TRUE ))

   } else {
     xm<-s[y,x]
     if(!is.na(xm)){ xsd<-0 }else {xsd<-NA }
   }
   return(xsd)                   #: run separately if neccessary?
 }
