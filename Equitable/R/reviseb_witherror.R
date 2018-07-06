reviseb_witherror <-
function(rownum=NULL,Td,ref=ncol(Td$ET.x)) {
  b_old<-Td$ET.Eb
  b_old1<-Td$E.b
  s<-Td$ET.Es
  s1<-Td$E.s
  oldzero<-Td$l.s.zero
  if(is.null(ref))ref<-ncol(Td$ET.x)
  if(!is.null(rownum)) zero<-Td$ET.x[rownum,ref]-oldzero else zero<-0 #unchanged if rownum not set
  # cat("\nref ",ref)
  # cat("\nold zero ", oldzero," orig data zero ", Td$ET.x[rownum,ref]," final zero set to ",zero)
  Td$ET.Eb<-b_old-zero*(1-s)
  Td$E.b<-b_old1-zero*(1-s1)
  ssd<-Td$E.sd1 #/sqrt(Td$E.sN)
    bsd<-Td$E.bsd1 #/sqrt(Td$E.bN)
    zsd<-Td$ET.xsd[rownum,ref]/sqrt(Td$ET.EN[rownum,ref])
  #Td$E.bsd1<-sqrt(((s-1)*ssd)^2 +((zero-oldzero)*zsd)^2+bsd^2)    #
   #Td$E.bsd1<-sqrt(((zero)*zsd)^2+bsd^2)
  #Td$E.bsd1<-sqrt( +bsd^2)



 Td$E.bsd1<-sqrt(((s-1)*ssd)^2 +bsd^2)    #errors due to slopes and intercepts included sqrt( (dF/ds*ssd)^2+(dF/db*bsd)^2 )
  Td$l.s.zero<-Td$ET.x[rownum,ref]
 # imagenan(Td$E.bsd1)
  return(Td)
}
