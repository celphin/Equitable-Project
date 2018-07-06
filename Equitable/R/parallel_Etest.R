parallel_Etest <-
function(Es,p,p_par,slope=1,scritical=0.1,pcritical=1e-5){
  imagenan(p_par,zlim=c(0.01,0.05),outside.above.color='red',main="Enter Etest :Parallel=red")
  #imagenan(p,main="Prob for no correlation")
  for (r in 1:nrow(Es)){
    for (c in 1:ncol(Es)){
      pv<-p[r,c]
      if(!is.na(pv)&& !is.nan(pv) && !is.infinite(pv)){
    if(pv<pcritical && !is.na(Es[r,c])  && !is.nan(Es[r,c])){

      if( abs((slope-Es[r,c])/slope)<scritical)p_par[r,c]=1 else p_par[r,c]=0
    }
      }
    }
  }
  imagenan(p_par,zlim=c(0.01,0.05),outside.above.color='red',main="Exit Etest :Parallel=red")
  cat("\n(At 95% Confidence: Proportion of parallel slopes(NULL)",(length(which(p_par>0.05)) )/length(p_par[which(!is.na(p_par))]) ) #parallel            .95
  cat(" Non parallel ",(length(which(p_par<=0.05)) )/length(p_par[which(!is.na(p_par))]) ,"\n") #not parallel 95%
  cat("    98%: Proportion of parallel slopes ",(length(which(p_par>0.02)) )/length(p_par[which(!is.na(p_par))]) ) #parallel            .95
  cat(" Non parallel ",(length(which(p_par<=0.02)) )/length(p_par[which(!is.na(p_par))]) ,"\n\n") #not parallel 95%
  return(p_par)
  }
