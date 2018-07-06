Eslope <-
function(y,x,s,nc,p){
   s<-matrix(s,nrow=nc,ncol=nc) ;p<-matrix(p,nrow=nc,ncol=nc)
   #print(s)
   sx_=s[x,]; sy_<- s[y,]; py_<-p[y,]; px_<-p[x,]
   j<-which(complete.cases(sx_,sy_))
   if(length(j)>0){
    xvect<-sy_[j]/sx_[j]
    wvect<-(1-py_[j])^2*(1-px_[j])^2
     xm<-weighted.mean(xvect, wvect, na.rm = TRUE)

   } else {
     xm<-s[y,x]
   }

   return(xm)                   #may not be able to get sd from this: run separately if neccessary?
 }
