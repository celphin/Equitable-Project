liner <-
function(x,y,mat,minnum=4,minsd=10e-10) {
  fy<-mat[,y] ; fx<-mat[,x]
  ncomp<-length(which(complete.cases(fy,fx)))
  if(ncomp>=minnum && abs(sd(fx, na.rm=TRUE))>minsd){
    fit<-lm(fy~fx, na.action=na.exclude )
    fit_coef<-coef(summary(fit))
    if(dim(fit_coef)==2) {
      p_par<-parallel_test(fx,fy)
    param<-list(
      s=fit_coef[2,"Estimate"],sse=fit_coef[2,"Std. Error"],
      b=fit_coef["(Intercept)","Estimate"], bse=fit_coef["(Intercept)","Std. Error"],
      r2=summary(fit)$r.squared,
      N=summary(fit)$df[2]+2,
      pslope=fit_coef[2,"Pr(>|t|)"],
      node=0,
      p_par=p_par
    )

    } else{
      param<-list(
        s=NA,sse=NA,
        b=NA, bse=NA,
        r2=NA,
        N=NA,
        pslope=NA,
        node=1,
        p_par=NA
      )
    }
  } else {
    param<-list(s=NA,sse=NA, b=NA, bse=NA,r2=NA, N=ncomp, pslope=NA,node=1,p_par=NA)
  }
  return(param)
}
