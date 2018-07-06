reduce_s <-
function(s,se,p,ls,lse ,node,minp=0.5,mins=0,b=FALSE){

    blankthreshold<-0.5
    s1<-s
    s1[(p>minp | (abs(s1)-se)<mins)]<-NA
    ls[(p>minp | (abs(ls)-lse)<mins)]<-NA
    sp<-s1

    s1[which(is.na(t(s1)))]<-NA
    ls[which(is.na(t(ls)))]<-NA

    blankcols<-apply(s1, 1,FUN=function(x) sum(is.na(x)))/ncol(s1)
    if(b){        #this is fundamental to correct functioning of transform
      s1[which(blankcols>blankthreshold),]<-ls[which(blankcols>blankthreshold),]     #these rows have almost all slopes of zero so set them to zero
      node[which(blankcols>blankthreshold),]<-2    #need to reset intercept as well
      s1[,which(blankcols>blankthreshold)]<-NA   #ignore infinite slopes
    } else {
      s1[which(blankcols>blankthreshold),]<-NA
   #   imagenan(s1,zlim=c(0,4),main=" Final:Remove full columns",xlab="Row", ylab="Col")
      #image(s1,zlim=c(0,row.3rd[nrow(s)]+1),main="Final: Remove transpose elements",xlab="Row", ylab="Col")
      s1[,which(blankcols>blankthreshold)]<-NA
    }
    s1[row(s1)==col(s1)]<-1
   # imagenan(s1,zlim=c(0,4),main=" Final:Inside Remove transpose rows",xlab="Row", ylab="Col")

     s1andnode<-list(s1=s1,node=node)
    return(s1andnode)

  }
