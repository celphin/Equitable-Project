Esdintercept <-
function(y,x,b,s,nc,p){                                  # find approximation to equitable using vector of reference column slopes
   b<-matrix(b,nrow=nc,ncol=nc) ;s<-matrix(s,nrow=nc,ncol=nc) ;p<-matrix(p,nrow=nc,ncol=nc)
   #print(b)
   bx_=b[x,]; by_<- b[y,];syx<-s[y,x]; py_<-p[y,]; px_<-p[x,]; pyx<-p[y,x]   #these run up Nov 30/16
   #b_x=b[,x]; by_<- b[y,];sy_<-s[y,]; py_<-p[y,]; p_x<-p[,x]
   j<-which(complete.cases(bx_,by_))    #these run up Nov 30/16
   #j<-which(complete.cases(b_x,by_,sy_))
   if(length(j)>0 && !is.na(syx)){  #this run to Nov 30/16
     # if(length(j)>0 ){
     # x[i] <- old_i[y,i]-s[y,u]*old_i[u,i]       #oldest ran this way until dec 24/15
     # wt[i] <- (1-ps[y,i])*(1-ps[u,i])*(1-ps[y,u])  #up to Jan 7 ps u
     xvect<- by_[j]-syx*bx_[j]
     wvect<-(1-py_[j])*(1-px_[j])^2*(1-pyx)^2   #these run up Nov 30/16
     # xvect<- by_[j]+sy_[j]*b_x[j]
     # wvect<-(1-py_[j])*(1-py_[j])^2*(1-p_x[j])^2
     xm<-weighted.mean(xvect, wvect, na.rm = TRUE)
     xsd<-sqrt(sum(wvect * (xvect - xm)^2,na.rm = TRUE ))
   } else {
     xm<-b[y,x]
     if(!is.na(xm)) xsd<-0 else xsd<-NA
   }

   return(xsd)                   #may not be able to get sd from this: run separately if neccessary?
 }
