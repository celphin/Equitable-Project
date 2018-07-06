nastat <-
function(d){
  m<-mean(c(d),na.rm = TRUE )
  std<-sd(c(d),na.rm = TRUE )
  cat("mean: ",m)
  cat(" std. dev.: ",std,"\n" )   #
  ms<-list(m=m,std=std)
  return(ms)
}
