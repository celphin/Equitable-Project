findnonzerocolumns <-
function(x){
  #xrange<-abs(min(c(x),na.rm = TRUE)-max(c(x),na.rm = TRUE))
  yrange<-NULL
  for(c in 1:ncol(x)){yrange<-c(yrange,abs(min(x[,c],na.rm = TRUE)-max(x[,c],na.rm = TRUE)))}
  xrange<-max(yrange)
  nonzero<-which(yrange/xrange>=0.05)
  ze<-which(yrange/xrange<0.05)
  return(nonzero)
}
