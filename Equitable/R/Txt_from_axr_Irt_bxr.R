Txt_from_axr_Irt_bxr <-
function(axr,Irt,bxr=NULL,zero=0,maxA=NULL){
  #axr varies over a, Irt varies over t , bxr varies over x
  #need to not run slope when larger than 3?
  if(!is.null(maxA)) axr[axr>maxA]<-NA
  if(is.null(bxr)){bx<-rep(0,length(axr))}
  Ixt<-matrix(NA,nrow=length(Irt),ncol=length(axr))
  colnames(Ixt)<-names(axr)
  rownames(Ixt)<-names(Irt)
 for(t in 1:length(Irt)){Ixt[t,]<-axr*(Irt[t]-zero)+bxr}
    imagenan(Ixt,main="Data built from a(x,r), b(x,r), I(r,t)")
    Ixt<-Ixt+zero
  return(Ixt)
}
