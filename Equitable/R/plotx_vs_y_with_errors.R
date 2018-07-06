plotx_vs_y_with_errors <-
function( x, y,stdy, xlab="Reference", ylab="non-Reference",main="Non-reference Vs Reference",
                                   ylim=c(140,260),rnames=names(y),
                                   pch=15,cex.main=0.8){
  #dataset and data_std are the data vector and error bars respectively
  numrows <- length(y)
  d = data.frame(
    x  = x
    , y  = y
    , sd = stdy
  )

  plot(d$x, d$y ,xlim= ylim,ylim= ylim,xlab=xlab,ylab=ylab)   #,main=main
  with (
    data = d
    , expr = errbar(x, y, y+sd, y-sd,add=TRUE,  pch=pch, cap=.01)
  )
 title(main=main,cex.main=cex.main)



  lines(ylim[1]:ylim[2],rep(0,length(ylim[1]:ylim[2])))
  lines(rep(0,length(ylim[1]:ylim[2])),ylim[1]:ylim[2])
  lines(ylim[1]:ylim[2],ylim[1]:ylim[2],lty=2)
  return()
}
