reviseb <-
function(zero,b_old,s) {
  cat("\n",ncol(b_old))
  cat("\n",ncol(s))
  cat("\n",zero)
  b<-b_old-zero*(1-s)
  return(b)
}
