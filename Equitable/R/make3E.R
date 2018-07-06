make3E <-
function(x){
  Td1<-transformE(x,Ave=FALSE,diagonal=FALSE)
  plotsummary(Td1)
  x1<-Td1$smat-Td1$ET.x
  Td2<-transformE(x1,Ave=FALSE,diagonal=FALSE)
  plotsummary(Td2)
  x2<-Td2$smat-Td2$ET.x
  Td3<-transformE(x2,Ave=FALSE,diagonal=FALSE)
  plotsummary(Td3)
  x3<-Td3$smat-Td3$ET.x
  T3<-list(Td1=Td1,Td2=Td2,Td3=Td3,resid=x3)
  return(T3)
}
