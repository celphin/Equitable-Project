calc_pcaOLD <-
function(x=Td_noise$smat,main=" ",sectors.f=NULL){
  #if(fcol)col<-col else col<-"black"

  newdata<-as.data.frame(x)
  # newdata <- scale(newdata, center = TRUE, scale = TRUE)   #x<-Td$smat   imagenan(newdata)
  # p = princomp(na.omit(newdata)) #attributes(p)    omits cols with na
  # loadings = p$loadings[]
  # p.variance.explained = p$sdev^2 / sum(p$sdev^2)
  # # plot percentage of variance explained for each principal component
  # barplot(100*p.variance.explained,  xlim=c(0,10), ylab='% Variance Explained')
  # runs finite mixture model
  if(ncol(newdata)>5){
    fmm<-Mclust(na.omit(newdata) )  #fmm<-Mclust(ratings)
    fmm
    table(fmm$classification)
    #fmm$parameters$mean

    # compares with k-means solution
    kcl<-kmeans(na.omit(newdata), 4, nstart=25)
    print(table(fmm$classification, kcl$cluster))
    cat("\npartition into 4 clusters")
    aa<-sapply(1:4,function(j){ cat("\n Cluster ",j,"\n",names(kcl$cluster)[which(kcl$cluster==j)],"\n") } ) #j<-1

    colnames(newdata)<-paste0("p",1:ncol(newdata))   # for(c in 1:ncol(newdata)){ cat("\n",sd(newdata[,c],na.rm=TRUE))}
    res.cov<-cov(newdata)
    eig<-eigen(res.cov)
    imagenan(eig$vectors,main=paste("Eigenvectors",main))
    rownames(eig$vectors)<-colnames(x)
    plot(eig$vectors[,1],type="b",pch=15,main="1st Principal Component Eigenvector")
    plot(eig$vectors[,2],type="b",pch=15,main="2nd Principal Component Eigenvector")  # colnames(Td2$E.s) colnames(Td2$smat)
    plot(eig$vectors[,3],type="b",pch=15,main="3rd Principal Component Eigenvector")
    #xref<-which(min(Td$E.s[,1],na.rm=TRUE)==Td$E.s[,1])[1]
    # plot(Td$E.s[,xref],type="b",pch=15,main="Slope along  Reference") #imagenan(Td2$ET.Es)   x<-Td$smat

    # 5. compute the new dataset :
    #
    # Transpose eigeinvectors
    eigenvectors.t <- t(eig$vectors)
    # Transpose the adjusted data
    # newdata.t <- t(x)  #imagenan(newdata.t)
    newdata.t <- t(newdata)
    # The new dataset
    df.new <- eigenvectors.t %*% newdata.t
    # Transpose new data ad rename columns
    df.new <- t(df.new)
    colnames(df.new) <- paste0("PC",1:ncol(df.new)) #c("PC1", "PC2", "PC3", "PC4")
    imagenan(df.new[,1:3],main="1st 3 Principal Components (Equiv to slopes)")
    plot(df.new[,1],type="b",pch=15,main="1st Principal Component DATA")
    plot(df.new[,2],type="b",pch=15,main="2nd Principal Component DATA")  # colnames(Td2$E.s) colnames(Td2$smat)
    plot(df.new[,3],type="b",pch=15,main="3rd Principal Component DATA") #looks like scaled data

    #perhaps use df.new[,1] as ne average g(t) at min f(x)
    xref<-which(min(Td$E.s[,1],na.rm=TRUE)==Td$E.s[,1])[1]
    tref<-which(min(Td$smat[1,],na.rm=TRUE)==Td$smat[1,])[1]
    plot(Td$E.s[,xref],type="b",pch=15,main="Slope along  Reference") #imagenan(Td2$ET.Es)   x<-Td$smat
    plot(Td$E.s[,xref],eig$vectors[,1])
    #   #make equitbale matrix from this eigenvector
    A<- matrix(NA,nrow=nrow(eigenvectors.t), ncol=nrow(eigenvectors.t))
    B<- matrix(NA,nrow=nrow(eigenvectors.t), ncol=nrow(eigenvectors.t))
    Itave<-colMeans(x,na.rm=TRUE)
    plot(Itave)
    #B[,xref]= Itave
    B[,xref]<- Itave-Td$E.s[,xref]*Itave[xref]
    plot(Td$E.b[,xref])
    plot(Td$E.b[,tref])
    sapply(1:ncol(B),function(c){plot(Td$E.b[,c],main=paste(c))})
    B<-sapply(1:ncol(B),function(c){B[,c]<-B[,xref]-A[,c]*B[c,xref]})  # plot(A[,xref]) #currently wrong reference
    A[,xref]= eigenvectors.t[1,]   #plot(a[,xref])
    A<-sapply(1:ncol(A),function(c){A[,c]<-A[,xref]/A[c,xref]})  # plot(A[,xref])

    imagenan(A)
    imagenan(B);imagenan(Td$E.b)
    orig<-t(Td$smat)
    Tdata<-A %*%(orig-Td$l.s.zero) /ncol(A) #  nrow(Tdata)  ncol(Tdata) imagenan(t(e1))
    imagenan(Tdata)
    # plot(Tdata,t(e1));   plot(Td$ET.x,(e1),main="T vs e1")   #way better
    # sd(Tdata-t(e1))     # sd(Td$ET.x-(e1))
    sd(Tdata-t(Td$smat))
    ones<-matrix(rep(1,ncol(Td$ET.x)*nrow(Td$ET.x)),nrow=ncol(Td$ET.x),ncol=nrow(Td$ET.x))
    bval<-Td$E.b%*%ones/ncol(Td$E.b) ; imagenan(bval);
    Tdata<-Tdata+bval + Td$l.s.zero  #works

    #bval<-B%*%ones/ncol(Td$E.b) ; imagenan(bval);
    Tdata<-A %*%orig /ncol(A)+ B%*%ones/ncol(Td$E.b)  #
    imagenan(Tdata); sd(Tdata-t(Td$smat)) ; imagenan(t(Td$smat))

    #AA<-Td$ET.Es
    # Tsim<-crossprod(AA/Td$ET.EN,orig)
    # imagenan(Tsim); sd(Tsim-t(e1))
    AA<-Td$E.s #imagenan(Td$E.s)
    #AA[is.na(Td$ET.Es)] <- 0   #this doesnt work since 0 values are aeraged in

    #could include zero value in calc

    Tsimple<-AA %*%(orig-Td$l.s.zero) /ncol(Td$E.s)  #imagenan(Td$ET.Es)
    ones<-matrix(rep(1,ncol(Td$ET.x)*nrow(Td$ET.x)),nrow=ncol(Td$ET.x),ncol=nrow(Td$ET.x))
    bval<-Td$E.b%*%ones/ncol(Td$E.b) ; imagenan(bval);
    Tsimple<-Tsimple+bval + Td$l.s.zero  #  imagenan(Td$E.b)
    imagenan(Tsimple); sd(Tsimple-t(Td$smat))  # imagenan(t(Td$smat))
    imagenan(Tsimple); sd(Tsimple-t(e1))
    plot(Tsimple,t(Td$smat),main="Tsimple vs e1")    #plot(Tsimple,t(e1),main="Tsimple vs e1")
    imagenan( Td$Ave.ET.x); sd(Td$Ave.ET.x-(e1))
    plot(Td$Ave.ET.x,(e1),main="TAve vs e1");  plot(Td$smat[,"Row_Ave"])

    pca<-PCA(na.omit(newdata),graph=FALSE)   #  pca<-PCA(na.omit(newdata))

    print( fviz_eig(pca, addlabels = TRUE, main=paste("Scree Plot",main))  ) # ylim = c(0, 50),main="test"
    #barplot(100*pca$eig[,ncol(pca$eig):1]/sum(pca$eig))   # ncol(pca$eig):1
    # plot(cumsum(pca$eig[,ncol(pca$eig):1]), xlab = "Principal Component",
    #             ylab = "Cumulative Proportion of Variance Explained",
    #             type = "b")
    # #cumulative scree plot
    # > plot(cumsum(prop_varex), xlab = "Principal Component",
    #        ylab = "Cumulative Proportion of Variance Explained",
    #        type = "b")

    #fviz_eig(pca)
    print( fviz_pca_ind(pca,
                        col.ind = "cos2", # Color by the quality of representation
                        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                        repel = TRUE     # Avoid text overlapping
    ))
    print(fviz_pca_var(pca,
                       col.var = "contrib", # Color by contributions to the PC
                       gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                       repel = TRUE     # Avoid text overlapping
    ))

    plot(pca, choix=c("ind"), label="none", col.ind=fmm$classification,cex=3)
    legend('bottom', cex=1.,  legend =paste("General Clusters for",main))

    plot(pca, choix=c("ind"), label="none", col.ind=kcl$cluster,cex=3)
    clu<-factor(kcl$cluster)
    summary(clu)
    legend("topleft", cex=1, legend=paste("Cluster",levels(clu)),fill=levels(clu))
    legend('bottom', cex=1.,  legend =paste("4 Clusters for",main))

    if(!is.null(sectors.f)){
      summary(sectors.f)
      cols = as.double(sectors.f)
      names(cols)<-paste0("y_",names(sectors.f))
      plot(pca, choix=c("ind"), label="none", col.ind=cols[names(kcl$cluster)],cex=3)
      #pca$ind[,c("Dim.1","Dim.2")]
      legend('topleft', cex=1.,  legend =paste("Region",levels(sectors.f)) , fill = 1:nlevels(sectors.f), merge = F, bty = 'n')
      legend('bottomright', cex=1.,  legend =paste(" Regions for",main))
    }
    #slplot(pca, scoresLoadings = c(T,T)) , scol = wineClasses)
    # winePCAmethods <- pca(wine[,-1], scale = "uv", center = T, nPcs = 2, method = "svd")   #pcaMethods
    # slplot(winePCAmethods, scoresLoadings = c(T,T), scol = wineClasses)
    numC<-3
    # fit <- pca(na.omit(newdata), scale = "uv", center = TRUE, nPcs =numC,   method="ppca")  #pcaMethods
    # fit <- pca(na.omit(newdata), scale = "uv", center = TRUE,   method="ppca")  #pcaMethods
    fit <- pca(na.omit(newdata), scale = "uv", nPcs =numC, center = TRUE)
    # plot(fit,main=main)
    # pca(newdata,  method = "ppca")
    #  fit <- pca(newdata,  method="ppca")  #pcaMethods
    # slplot(fit, scoresLoadings = c(T,T), main=paste("PCA with ",numC," Comp.",main))   #slplot(fit, scoresLoadings = c(T,T), scol = wineClasses)
    #plot.pcaRes(fit, y = NULL, main = deparse(substitute(fit)))
    #slplot(fit, main=paste("PCA with ",numC," Comp.",main))
    plotPcs(fit, pcs = 1:nP(fit), type = c("scores", "loadings"),
            sl = NULL, hotelling = 0.5,col=kcl$cluster,pch=15,cex=1.5)   # hotelling = 0.95 pca methods?

    # source("https://bioconductor.org/biocLite.R")
    # biocLite("pcaMethods")
    # library(pcaMethods)
    cat("\n",main,"\n")
    print(summary(fit) )# print variance accounted for
    #print(loadings(fit)) # pc loadings
    #plot(fit,ylim=c(0,fit$sdev[1]^2),main=paste("PCA")) # scree plot
    biplot(fit,main=paste("PCA with ",numC," Comp.",main),var.axes=TRUE,lwd=4)
    # plot.pcaRes(fit)
    # plot.PCA(fit)
    # new("pcaRes", scores=[the scores],
    #     loadings=[the loadings],nPcs=[amount of PCs],
    #     R2cum=[cumulative observations], nVar=[amount of variables],
    #     R2=[R2 for each individual PC], sDev=[stdev for each individual calculate PCA],
    #     missing=[amount of NAs],completeObs=[estimated complete observations])
    #
    # fit <- princomp(x, cor=TRUE,scores =TRUE )
    # print(summary(fit) )# print variance accounted for
    # loadings(fit) # pc loadings
    # #plot(fit$sdev,ylim=c(0,5)) # scree plot
    # plot(fit,ylim=c(0,fit$sdev[1]^2),main=paste("PCA:", main)) # scree plot
    # fit$scores # the principal components
    # biplot(fit,xlim=c(-0.2,0.2),ylim=c(-0.2,0.2),main=paste("PCA:", main,"\n"),var.axes=TRUE)
  }
}
