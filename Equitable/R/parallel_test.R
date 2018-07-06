parallel_test <-
function(x,y,slope=1,scritical=0.1,pcritical=1e-5){

  fit <- lm(y ~ x, na.action=na.exclude )
  # Compute Summary with statistics
  sfit<- summary(fit)
  # Compute t- H0: intercept=slope. The estimation of coefficients and their s.d. are in sfit$coefficients
  if(nrow(sfit$coefficients)==2) {
  tstats <- (slope-sfit$coefficients[2,1])/sfit$coefficients[2,2]
  # Calculates two tailed probability
  pval<- 2 * pt(abs(tstats), df = df.residual(fit), lower.tail = FALSE)
  if(sfit$coefficients[2,"Pr(>|t|)"]<pcritical ){
    if( abs((slope-sfit$coefficients[2,1])/slope)<scritical)pval=1 else pval=0
  }
  } else pval<-NA
  if(is.nan(pval))pval<-NA
  #print(pval)
  #cat("\n p value for the null hypothesis that the slope is a slope of 1 ",pval)
  return(pval)
}
