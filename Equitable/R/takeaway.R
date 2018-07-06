takeaway <-
function(e1){
  rm<-rowMeans(e1)
  cm<-colMeans(e1)
  me<-mean(e1)
  for(r in 1:nrow(e1))for(c in 1:ncol(e1))e1[r,c]<-e1[r,c]-rm[r]-cm[c]+me
  imagenan(e1)
  return(e1)
}
