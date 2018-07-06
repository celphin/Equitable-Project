stats_residualsold <-
function(Td_noise,
                          Td=NULL,Td_old=NULL,genname="Equitable",ylim=NULL,pf=TRUE,ipf=TRUE){
  if(is.null(Td_old))Td_old<-Td_noise
  nc<-ncol(Td_noise$smat);nr<-nrow(Td_noise$smat)
  xresiduals<-Td_old$smat-Td_noise$ET.x
  sdI_T<-sd(xresiduals,na.rm=TRUE)
  if(is.null(ylim)){
    ylim<-c(min(xresiduals,na.rm=TRUE),max(xresiduals,na.rm=TRUE))
  }
  dam1<-c(NA,length=nrow(Td_noise$ET.Es))
  for (r in 1:nrow(Td_noise$ET.Es))dam1[r]<-weighted.mean(Td_noise$ET.Es[r,],(1-Td_noise$ET.Ep[r,]),na.rm = TRUE)-1
  # #for (r in 1:nrow(Td_noise$ET.Es))dam1[r]<-weighted.mean(Td_noise$E.s[r,],(1-Td_noise$E.pslope[r,]),na.rm = TRUE)-1
  x<-mean(dam1^2,na.rm = TRUE)
  #F<-sqrt((1)/ncol(noise)+1/nrow(noise))
  Fp<-sqrt((1+x)/nc+1/nr)
  F<-sqrt((1)/nc+1/nr)
  sdnoise_simple<-sdI_T/sqrt(1-F^2)
  sdnoise_approx<-sdI_T/sqrt(1-Fp^2)
  cat("\n Simple noise std dev from Transform is ",sdnoise_simple )
  cat("\n Approximate noise std dev from Transform is ",sdnoise_approx )

  if(pf){
    plot(Td_noise$l.s.x, Td_old$smat-Td_noise$l.s.x,
         main=paste0("Comparing  Residuals vs ",genname," L.S. Transform"),
         xlab = "Transform Values", ylab = paste0("Residuals(Data - ",genname," L.S. Transform)"),ylim=ylim)
    abline(h=0, lty=2,lwd=2)
    lines(smooth.spline(Td_noise$l.s.x, Td_old$smat-Td_noise$l.s.x),lwd=4,lty=1)

    plot(Td_noise$ET.x, xresiduals,
         main=paste0("Comparing  Residuals vs ",genname," Transform"),
         xlab = "Transform Values", ylab = paste0("Residuals (Data - ",genname," Transform)"),ylim=ylim)
    abline(h=0, lty=2,lwd=2)
    lines(smooth.spline(Td_noise$ET.x, xresiduals),lwd=4,lty=1)
  }
  if(!is.null(Td)){  #signal defined
    noise<-Td_old$smat-Td$smat
    sd_noise<-sd(noise,na.rm=TRUE)
    cat("\n correct noise std dev is", sd_noise)
    cm<-colMeans(noise,na.rm=TRUE)
    rm<-rowMeans(noise,na.rm=TRUE)
    N_Ave<-matrix(NA,nrow=nrow(noise),ncol=ncol(noise))
    for (r in 1:nrow(N_Ave))for(c in 1:ncol(N_Ave))N_Ave[r,c]<-rm[r]+cm[c]
    N_Avet<-matrix(NA,nrow=nrow(noise),ncol=ncol(noise))
    for (r in 1:nrow(N_Ave))for(c in 1:ncol(N_Ave))N_Avet[r,c]<-rm[r]*dam1[c]   #uses slopes from transform of noisy data
    sqrt(sd(N_Avet,na.rm=TRUE)^2+sd(N_Ave,na.rm=TRUE)^2)
    N_Ave_plus<-N_Ave+N_Avet
    sd(N_Ave_plus,na.rm=TRUE)
    xSresiduals<-Td$smat-Td_noise$ET.x
    if(pf){
      plot(Td_noise$l.s.x, Td$smat-Td_noise$l.s.x,
           main=paste0("Comparing  Residuals from signal vs ",genname," L.S. Transform"),
           xlab = "Transform Values", ylab = paste0("Residuals (Signal - ",genname," L.S. Transform)"),ylim=ylim)
      abline(h=0, lty=2,lwd=2)
      lines(smooth.spline(Td_noise$l.s.x, Td$smat-Td_noise$l.s.x),lwd=4,lty=1)

      plot(Td_noise$ET.x, xSresiduals,
           main=paste0("Comparing  Residuals from signal vs ",genname," Transform"),
           xlab = "Transform Values", ylab = paste0("Residuals (Signal - ",genname," Transform)"),ylim=ylim)
      abline(h=0, lty=2,lwd=2)
      lines(smooth.spline(Td_noise$ET.x, xSresiduals),lwd=4,lty=1)
    }
    sdN_Ave<-sd(N_Ave,na.rm=TRUE)
    N_AveAprox<- (-1)*xSresiduals
    theory_sdN_Ave<-F*sd_noise
    theory_sdN_Ave_plus<-Fp*sd_noise
    sdN_AveAprox<-sd(N_AveAprox,na.rm=TRUE)    #T-S ~ N_Ave
    sdN_Ave_plus<-sd(N_Ave_plus,na.rm=TRUE)    #
    cat("\nstd of 2D noise averages is ",sdN_Ave)
    cat("\nstd of 2D noise averages+extra from slopes is ",sdN_Ave_plus)
    cat("\nTHEORY std of 2D noise averages is ",theory_sdN_Ave)
    cat("\nTHEORY std of 2D noise averages+extra from slopes is ",theory_sdN_Ave_plus)
    cat("\n Approx of std of 2D noise averages frorm trasform is ",sdN_AveAprox )
    if(ipf){
      imagenan(N_Ave,zlim=c(min(N_Ave,na.rm=TRUE),max(N_Ave,na.rm=TRUE) ),main="N_Ave= N(x)_tave + N_xave(t)")
      imagenan(N_Ave_plus,zlim=c(min(N_Ave,na.rm=TRUE),max(N_Ave,na.rm=TRUE) ),main="N_Ave_plus= N(x)_tave +dam1[y]xave* N_xave(t)+CS")
      imagenan(N_AveAprox,zlim=c(min(N_Ave,na.rm=TRUE),max(N_Ave,na.rm=TRUE) ),main=" Transform-Signal ~ N(x)_tave +(1+dam1[y]xave)* N_xave(t)")
    }
    if(pf){
      # plot(N_AveAprox, N_Ave-N_AveAprox,
      #      main=paste0("Comparing  Residuals of Noise averages vs ",genname," T-S"),
      #      xlab = "Transform Values", ylab = paste0("Residuals (Row Col Noise Ave - ",genname," T-S)"),ylim=ylim)
      # abline(h=0, lty=2,lwd=2)
      # lines(smooth.spline(N_AveAprox, N_Ave-N_AveAprox),lwd=4,lty=1)

      plot(N_AveAprox, N_Ave_plus-N_AveAprox,
           main=paste0("Comparing  Residuals of Noise averages(plus) vs ",genname," T-S"),
           xlab = "Transform Values", ylab = paste0("Residuals (Row Col Noise Ave(plus) - ",genname," T-S)"),ylim=ylim)
      abline(h=0, lty=2,lwd=2)
      lines(smooth.spline(N_AveAprox, N_Ave_plus-N_AveAprox),lwd=4,lty=1)
    }
  }

  stdvalues<-list(
    sd_noise=sd_noise, sdnoise_simple=sdnoise_simple ,sdnoise_approx=sdnoise_approx ,sdN_AveAprox=sdN_AveAprox,
    sdN_Ave=sdN_Ave, sdN_Ave_plus=sdN_Ave_plus,
    theory_sdN_Ave=theory_sdN_Ave, theory_sdN_Ave_plus=theory_sdN_Ave_plus)
  return(stdvalues)
}
