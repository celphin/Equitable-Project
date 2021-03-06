stats_residuals <-
function(Td_noise,
                          Td=NULL,Td_old=NULL,genname="Equitable",ylim=NULL,pf=TRUE,ipf=TRUE,C=1.028){
 # cat("\n\n C= ", C)
  if(is.null(Td_old)){
    Td_old<-Td_noise
    } else{  # Td_noise has slopes that are not masked so mask them like in Td_old
      imagenan(Td_noise$ET.Es,zlim=c(-4,4),main="premask")
   Td_noise$ET.Es[is.na(Td_old$ET.Es)]<-NA
    imagenan(Td_noise$ET.Es,zlim=c(-4,4),main="postmask")
  }
  nc<-ncol(Td_noise$smat);nr<-nrow(Td_noise$smat)
  xresiduals<-Td_old$smat-Td_noise$ET.x
  sdI_T<-sd(xresiduals,na.rm=TRUE)
  if(is.null(ylim)){
    ylim<-c(min(xresiduals,na.rm=TRUE),max(xresiduals,na.rm=TRUE))
  }
  dam1<-c(NA,length=nrow(Td_noise$ET.Es))
  for (r in 1:nrow(Td_noise$ET.Es))dam1[r]<-weighted.mean((Td_noise$ET.Es[r,]),(1-Td_noise$ET.Ep[r,]),na.rm = TRUE)
  #for (r in 1:nrow(Td_noise$ET.Es))dam1[r]<-mean(abs(Td_noise$ET.Es[r,]),na.rm = TRUE)
  x<-mean((1-dam1)^2,na.rm = TRUE)
  #x<-mean((dam1)^2,na.rm = TRUE)
  #cat("\n mean of slope squared term is ", x)

  #F<-sqrt((C+x)/ncol(noise)+1/nrow(noise))
  #Fp<-sqrt((x)/nc+1/nr)
  Fp<-sqrt((C+x)/nc+1/nr)
  F<-sqrt((1)/nc+1/nr)
  sdnoise_simple<-sdI_T/sqrt(1-F^2)
  sdnoise_approx<-sdI_T/sqrt(1-Fp^2)
  #cat("\n Simple noise std dev from Transform is ",sdnoise_simple )
  #cat("\n Approximate noise std dev from Transform is ",sdnoise_approx )


  plot(Td_noise$l.s.x, Td_old$smat-Td_noise$l.s.x,
       main=paste0("Comparing  Residuals vs ",genname," L.S. Transform"),
       xlab = "Transform Values", ylab = paste0("Residuals(Data - ",genname," L.S. Transform)"),ylim=ylim)
  abline(h=0, lty=2,lwd=2)
  if(pf)lines(smooth.spline(Td_noise$l.s.x, Td_old$smat-Td_noise$l.s.x),lwd=4,lty=1)

  plot(Td_noise$ET.x, xresiduals,
       main=paste0("Comparing  Residuals vs ",genname," Transform"),
       xlab = "Transform Values", ylab = paste0("Residuals (Data - ",genname," Transform)"),ylim=ylim)
  abline(h=0, lty=2,lwd=2)
  if(pf)lines(smooth.spline(Td_noise$ET.x, xresiduals),lwd=4,lty=1)

  if(!is.null(Td)){  #signal defined
    noise<-Td_old$smat-Td$smat
    sd_noise<-sd(noise,na.rm=TRUE)
    cat("\n correct noise std dev is", sd_noise)
    cm<-colMeans(noise,na.rm=TRUE)
    rm<-rowMeans(noise,na.rm=TRUE)
    Srm<-rowMeans(Td$smat,na.rm=TRUE)
    N_Ave<-matrix(NA,nrow=nrow(noise),ncol=ncol(noise))
    for (r in 1:nrow(N_Ave))for(c in 1:ncol(N_Ave))N_Ave[r,c]<-rm[r]+cm[c]
    N_Avet<-matrix(NA,nrow=nrow(noise),ncol=ncol(noise))
    N_Ave_plus<-matrix(NA,nrow=nrow(noise),ncol=ncol(noise))
    #for (r in 1:nrow(N_Ave))for(c in 1:ncol(N_Ave))N_Avet[r,c]<-rm[r]*((-1+dam1[c]*sqrt(C)))   #uses slopes from transform of noisy data
     for (r in 1:nrow(N_Ave))for(c in 1:ncol(N_Ave))N_Ave_plus[r,c]<-cm[c]+rm[r]*(C+dam1[c])+(C-1)*(Srm[r]-mean(Td$smat,na.rm=TRUE))
    #sqrt(sd(N_Avet,na.rm=TRUE)^2+sd(N_Ave,na.rm=TRUE)^2)
   # N_Ave_plus<-N_Ave+N_Avet
    sd(N_Ave_plus,na.rm=TRUE)
    xSresiduals<-Td$smat-Td_noise$ET.x

    plot(Td_noise$l.s.x, Td$smat-Td_noise$l.s.x,
         main=paste0("Comparing  Residuals from signal vs ",genname," L.S. Transform"),
         xlab = "Transform Values", ylab = paste0("Residuals (Signal - ",genname," L.S. Transform)"),ylim=ylim)
    abline(h=0, lty=2,lwd=2)
    if(pf)lines(smooth.spline(Td_noise$l.s.x, Td$smat-Td_noise$l.s.x),lwd=4,lty=1)

    plot(Td_noise$ET.x, xSresiduals,
         main=paste0("Comparing  Residuals from signal vs ",genname," Transform"),
         xlab = "Transform Values", ylab = paste0("Residuals (Signal - ",genname," Transform)"),ylim=ylim)
    abline(h=0, lty=2,lwd=2)
    if(pf) lines(smooth.spline(Td_noise$ET.x, xSresiduals),lwd=4,lty=1)

    sdN_Ave<-sd(N_Ave,na.rm=TRUE)
    N_AveAprox<- (-1)*xSresiduals
    theory_sdN_Ave<-F*sd_noise
    theory_sdN_Ave_plus<-Fp*sd_noise+(C-1)*sd(Srm,na.rm=TRUE)
    sdN_AveAprox<-sd(N_AveAprox,na.rm=TRUE)    #T-S ~ N_Ave
    sdN_Ave_plus<-sd(N_Ave_plus,na.rm=TRUE)    #
    cat("\nstd of 2D noise averages is ",sdN_Ave)
    cat("\nstd of 2D noise averages+extra from slopes is ",sdN_Ave_plus)
    cat("\nTHEORY std of 2D noise averages is ",theory_sdN_Ave)
    cat("\nTRUE std of 2D noise averages is ",sdN_Ave)
    cat("\nTHEORY std of 2D noise averages+extra from slopes is ",theory_sdN_Ave_plus)
    cat("\n Approx of std of 2D noise averages from transform is ",sdN_AveAprox )
    if(ipf){
    imagenan(N_Ave,zlim=c(min(N_Ave,na.rm=TRUE),max(N_Ave,na.rm=TRUE) ),main="N_Ave= N(x)_tave + N_xave(t)")
    imagenan(N_Ave_plus,zlim=c(min(N_Ave,na.rm=TRUE),max(N_Ave,na.rm=TRUE) ),main="N_Ave_plus= cm[c]+rm[r]*(dam1[c]) +(C-1)*(Srm[r])")
    imagenan(N_AveAprox,zlim=c(min(N_Ave,na.rm=TRUE),max(N_Ave,na.rm=TRUE) ),main=" Transform-Signal ~ N(x)_tave +(dam1[y]xave)* N_xave(t)")


    # plot(N_AveAprox, N_Ave-N_AveAprox,
    #      main=paste0("Comparing  Residuals of Noise averages vs ",genname," T-S"),
    #      xlab = "Transform Values", ylab = paste0("Residuals (Row Col Noise Ave - ",genname," T-S)"),ylim=ylim)
    # abline(h=0, lty=2,lwd=2)
    # lines(smooth.spline(N_AveAprox, N_Ave-N_AveAprox),lwd=4,lty=1)

        ylim<-c(min(N_Ave-N_AveAprox,na.rm=TRUE),max(N_Ave-N_AveAprox,na.rm=TRUE))

    plot(N_AveAprox, N_Ave_plus-N_AveAprox,
         main=paste0("Comparing  Residuals of Noise averages(plus) vs ",genname," T-S"),
         xlab = "Transform Values", ylab = paste0("Residuals (Row Col Noise Ave(plus) - ",genname," T-S)"),ylim=ylim)
    abline(h=0, lty=2,lwd=2)
    if(pf) lines(smooth.spline(N_AveAprox, N_Ave_plus-N_AveAprox),lwd=4,lty=1)
    plot(N_AveAprox, N_Ave-N_AveAprox,
         main=paste0("Comparing  Residuals of Noise averages vs ",genname," T-S"),
         xlab = "Transform Values", ylab = paste0("Residuals (Row Col Noise Ave - ",genname," T-S)"),ylim=ylim)
    abline(h=0, lty=2,lwd=2)
    if(pf)lines(smooth.spline(N_AveAprox, N_Ave-N_AveAprox),lwd=4,lty=1)
    }
    stdvalues<-list(
      sd_noise=sd_noise, sdnoise_simple=sdnoise_simple ,sdnoise_approx=sdnoise_approx ,sdN_AveAprox=sdN_AveAprox,
      sdN_Ave=sdN_Ave, sdN_Ave_plus=sdN_Ave_plus,
      theory_sdN_Ave=theory_sdN_Ave, theory_sdN_Ave_plus=theory_sdN_Ave_plus)
  } else {
    stdvalues<-list(
     sdnoise_simple=sdnoise_simple ,sdnoise_approx=sdnoise_approx )
  }


  return(stdvalues)
}
