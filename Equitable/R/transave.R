transave <-
function(smat,Em,equita=TRUE,x=NULL,t=NULL,diagonal=TRUE,zero=0){

  d_ave<-xtdata(smat,x=x,t=t)

      d_ave<-d_ave-zero
      cat("\ntransave: zero used is ",zero)
  ETave<-transf(d_ave,Em,equita=equita,diagonal=diagonal)   #make Equitable transform  ET$x and std dev in ET$xsd
  names(ETave)<-paste0("Ave.ET.",names(ETave))
    ETave$Ave.ET.x<-ETave$Ave.ET.x+zero

  return(ETave)
}
