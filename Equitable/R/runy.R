runy <-
function(y,mat) {
      valy<-sapply(1:ncol(mat),FUN= liner,y=y,mat=mat)
      v<-as.list(t(valy))
      return(v)
}
