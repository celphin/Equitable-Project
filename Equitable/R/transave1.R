transave1 <-
function(smat,TE,equita=TRUE,x=NULL,t=NULL,diagonal=TRUE,Zero=FALSE){
  Em<-list(TE$l.s.s ,TE$l.s.sse ,TE$l.s.b   ,
           TE$l.s.bse ,TE$l.s.r2 ,TE$l.s.N  ,TE$l.s.pslope ,TE$l.s.node, TE$E.s,TE$E.b )
  names(Em)<-c( "s","sse","b","bse","r2","N","pslope","node","p_par","E.s","E.b")
  if(Zero)zero<-TE$l.s.zero else zero=0
  cat("\ntransave1: zero used is ",zero)

  ETave<-transave(smat,Em,equita=equita,x=x,t=t,diagonal=diagonal,zero=zero)
  return(ETave)
}
