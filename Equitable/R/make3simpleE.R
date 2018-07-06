make3simpleE <-
function(T3){
  Td<-T3$Td1
  Tx1<-make_data_fromE(orig=Td$smat,A=Td$E.s,B=Td$E.b,zero=Td$l.s.zero, main="Direct Simple 1")#
  #attributes(Tx1)
  Td<-T3$Td2
  Tx2<-make_data_fromE(orig=Td$smat,A=Td$E.s,B=Td$E.b,zero=Td$l.s.zero, main="Direct Simple 2")#

  Td<-T3$Td3
  Tx3<-make_data_fromE(orig=Td$smat,A=Td$E.s,B=Td$E.b,zero=Td$l.s.zero, main="Direct Simple 3")#
  Tx<-list(e1=Tx1$Tx,e2=Tx2$Tx,e3=Tx3$Tx)
  return(Tx)
}
