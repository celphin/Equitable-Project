make_data_frompca <-
function(orig,main=" "){

  orig<-as.data.frame(orig)

  if(ncol(orig)>3){
    res.cov<-cov(orig)
    eig<-eigen(res.cov)
    imagenan(eig$vectors,main=paste("Eigenvectors",main))
    rownames(eig$vectors)<-colnames(orig)
    plot(eig$vectors[,1],type="b",pch=15,main="1st Principal Component Eigenvector")
    plot(eig$vectors[,2],type="b",pch=15,main="2nd Principal Component Eigenvector")  # colnames(Td2$E.s) colnames(Td2$smat)
    plot(eig$vectors[,3],type="b",pch=15,main="3rd Principal Component Eigenvector")

    newdata.t <- t(orig)
    # Transpose eigeinvectors
    eigenvectors.t <- t(eig$vectors)
    # The new dataset
    df.new <- eigenvectors.t %*% newdata.t  #looks like g(t)*sigma(f)^2
    # Transpose new data ad rename columns
    df.new <- t(df.new)
    colnames(df.new) <- paste0("PC",1:ncol(df.new)) #c("PC1", "PC2", "PC3", "PC4")
    imagenan(df.new[,1:3],main="1st 3 Principal Components (Equiv to slopes)")
    plot(df.new[,1],type="b",pch=15,main="1st Principal Component DATA:g1(t)*sigma(f1)^2")
    plot(df.new[,2],type="b",pch=15,main="2nd Principal Component DATA:g2(t)*sigma(f2)^2")  # colnames(Td2$E.s) colnames(Td2$smat)
    plot(df.new[,3],type="b",pch=15,main="3rd Principal Component DATA:g3(t)*sigma(f3)^2") #looks like scaled data

    # sapply(1:ncol(B),function(c){plot(Td$E.b[,c],main=paste(c))})

    # Tsimple<-make_data_fromE(orig=Td$smat,A=Td$E.s,B=Td$E.b,zero=Td$l.s.zero, main=" ")   #make data from simple slope intercept
    # zero=Td$l.s.zero
    # Torig1<-orig-t(Tsimple) #imagenan(Torig1,zlim=c(-0.1,0.1)); imagenan(orig); imagenan(e2+e3,zlim=c(-0.1,0.1))
    # sd(t(Tsimple)-Td$smat)
    #                                                #imagenan(as.matrix(Torig1),zlim=c(-0.1,0.1)) imagenan(Tsimple) imagenan(e1)
    # ETorig1<-orig-Td$ET.x   #imagenan(ETorig1,zlim=c(-0.2,0.2)); zlim<- c(min(orig-e1),max(orig-e1)) ;imagenan(orig-e1,zlim=zlim)
    eigenvect<-eigenvectors.t[1,]
    Tdata1<-make_datafrom_eigen(eigenvect= eigenvect,orig=orig,xref=1,main="1st Eigenvector")                  #imagenan(orig);imagenan(Tdata1) makedata from eigenvector and time average
    Tx1<-Tdata1$Tx
    eigenvect<-eigenvectors.t[2,]
    orig1<-orig-Tx1 #imagenan(orig1,zlim=c(-0.1,0.1));  imagenan(e2+e3,zlim=c(-0.1,0.1))

    Tdata2<-make_datafrom_eigen(eigenvect=eigenvect,orig=orig1,xref=2,sgn=(1),main="2nd Eigenvector")                  #makedata from eigenvector and time average
    Tx2<-Tdata2$Tx
    eigenvect<-eigenvectors.t[3,]
    orig2<-orig1-Tx2   #imagenan(Tdata2,zlim=c(-0.1,0.1))  ;imagenan(e2,zlim=c(-0.1,0.1))
                         # imagenan(orig2,zlim=c(-0.02,0.02));  imagenan(e3,zlim=c(-0.02,0.02))

    Tdata3<-make_datafrom_eigen(eigenvect=eigenvect,orig=orig2,xref=3,sgn=(1),main="3rd Eigenvector")                  #makedata from eigenvector and time average
    Tx3<-Tdata3$Tx
     Teigen<-list(e1=Tx1,e2=Tx2,e3=Tx3)   #E=Tsimple,
  }
  return(Teigen)
}
