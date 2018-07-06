bN <-
function(y,x,b,s,nc,p){                                  # find approximation to equitable using vector of reference column slopes
   b<-matrix(b,nrow=nc,ncol=nc) ;s<-matrix(s,nrow=nc,ncol=nc) ;p<-matrix(p,nrow=nc,ncol=nc)
   #print(b)
   bx_=b[x,]; by_<- b[y,];syx<-s[y,x]; py_<-p[y,]; px_<-p[x,]; pyx<-p[y,x]   #these run up Nov 30/16
   #b_x=b[,x]; by_<- b[y,];sy_<-s[y,]; py_<-p[y,]; p_x<-p[,x]
   j<-which(complete.cases(bx_,by_))    #these run up Nov 30/16
   #j<-which(complete.cases(b_x,by_,sy_))
   if(length(j)>0 && !is.na(syx)){  #this run to Nov 30/16
     N<-length(j)
   } else {
     xm<-b[y,x]
     if(!is.na(xm)) N<-1 else N<-NA
   }

   return(N)                   #may not be able to get sd from this: run separately if neccessary?
 }
