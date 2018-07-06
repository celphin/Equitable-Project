calc_prcomp <-
function(x=Td$smat,main=" "){
  op =
    par(mfrow =  c(1,1), mar = c(7,4.,4.5,4))
  imagenan(x, main=main)   #for( c in 1:ncol(x))cat("\n",length(which(!is.na(x[c,])))) if(sd(x[,c],na.rm=TRUE)==0)cat("\n",c)
  #for( c in 1:nrow(x))cat("\n",sd(x[c,],na.rm=TRUE))
  newdata<-as.data.frame(x)
  colnames(newdata)<-paste0("p",1:ncol(newdata))   # for(c in 1:ncol(newdata)){ cat("\n",sd(newdata[,c],na.rm=TRUE))}
  #cv<-cov(x, y = NULL, use = "pairwise.complete.obs",method = c("pearson"))

  fit<-prcomp(formula = ~., data = newdata  , scale = TRUE,  na.action=na.exclude)

  print(summary(fit) )# print variance accounted for
  print(loadings(fit)) # pc loadings
  plot(fit,ylim=c(0,fit$sdev[1]^2),main=paste("PCA")) # scree plot
  biplot(fit,main=paste("PCA",main),var.axes=TRUE,lwd=4)

  #
  # fit <- princomp(x, cor=TRUE,scores =TRUE )
  # print(summary(fit) )# print variance accounted for
  # loadings(fit) # pc loadings
  # #plot(fit$sdev,ylim=c(0,5)) # scree plot
  # plot(fit,ylim=c(0,fit$sdev[1]^2),main=paste("PCA:", main)) # scree plot
  # fit$scores # the principal components
  # biplot(fit,xlim=c(-0.2,0.2),ylim=c(-0.2,0.2),main=paste("PCA:", main,"\n"),var.axes=TRUE)
}
