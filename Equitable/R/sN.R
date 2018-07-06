sN <-
function(y,x,s,nc,p){                                  # find approximation to equitable using vector of reference column slopes
   s<-matrix(s,nrow=nc,ncol=nc) ;p<-matrix(p,nrow=nc,ncol=nc)
   #print(s)
   sx_=s[x,]; sy_<- s[y,]; py_<-p[y,]; px_<-p[x,]
   j<-which(complete.cases(sx_,sy_))

   if(length(j)>0){
     N<-length(j)

   } else {
     xm<-s[y,x]
     if(!is.na(xm)){ N<-1 }else {N<-NA }
   }
   return(N)                   #: run separately if neccessary?
 }
