egintersecttwo <-
function(rmult,cmult,days_between_sample=0,amp=40,amp2=20,A=200,A2=210,nfac=0,g0=0,g2=5){
  d<-egintersectday(10,5,amp=amp,g0=g0,A=A);plot(d[,20])
  d1<-egintersectday(10,5,amp=amp2,g0=g2,A=A2);plot(d1[,20]);lines(d[,20])
  colnames(d)<-paste0(colnames(d1),"_a",amp,"_g",g0,"_A",A)
  colnames(d1)<-paste0(colnames(d1),"_a",amp2,"_g",g2,"_A",A2)
  Tx<-cbind(d,d1)
  imagenan(Tx)
  return(Tx)
}
