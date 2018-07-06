minmax <-
function(d){
  mi<-min(c(d),na.rm = TRUE )
  ma<-max(c(d),na.rm = TRUE )
  cat("min: ",mi)
  cat(" max: ",ma,"\n" )   #
  ms<-list(mi=mi,ma=ma)
  return(ms)
  return(list(mi,ma))
}
