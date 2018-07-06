calc_pca <-
function(x=Td_noise$smat,main=" ",sectors.f=NULL,f_4=TRUE){
  #if(fcol)col<-col else col<-"black"
  newdata<-as.data.frame(x)
   if(ncol(newdata)>5){
  fmm<-Mclust(na.omit(newdata) )  #fmm<-Mclust(ratings)
  fmm
  table(fmm$classification)
  #fmm$parameters$mean
  # compares with k-means solution
  kcl<-kmeans(na.omit(newdata), 4, nstart=25)
  print(table(fmm$classification, kcl$cluster))
  cat("\n",main, " partition into 4 clusters")
  aa<-sapply(1:4,function(j){ cat("\n Cluster ",j,"\n",names(kcl$cluster)[which(kcl$cluster==j)],"\n") } ) #j<-1

  colnames(newdata)<-paste0("p",1:ncol(newdata))   # for(c in 1:ncol(newdata)){ cat("\n",sd(newdata[,c],na.rm=TRUE))}

   pca<-PCA(na.omit(newdata),graph=TRUE)   #  pca<-PCA(na.omit(newdata))

 print( fviz_eig(pca, addlabels = TRUE, main=paste("Scree Plot",main))  ) # ylim = c(0, 50),main="test"

 #  #fviz_eig(pca)
 # print( fviz_pca_ind(pca,
 #               col.ind = "cos2", # Color by the quality of representation
 #               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
 #               repel = TRUE     # Avoid text overlapping
 #  ))
 #  print(fviz_pca_var(pca,
 #               col.var = "contrib", # Color by contributions to the PC
 #               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
 #               repel = TRUE     # Avoid text overlapping
 #  ))

  plot(pca, choix=c("ind"), label="none", col.ind=fmm$classification,cex=3)
  legend('bottom', cex=1.,  legend =paste("General Clusters for",main))

  plot(pca, choix=c("ind"), label="none", col.ind=kcl$cluster,cex=3)
  clu<-factor(kcl$cluster)
  summary(clu)
  legend("topleft", cex=1, legend=paste("Cluster",levels(clu)),fill=levels(clu))
  legend('bottom', cex=1.,  legend =paste("4 Clusters for",main))

  if(!is.null(sectors.f)){
    cat("\nStart Sectors colouring for PCA\n")
  summary(sectors.f)
  cols = as.double(sectors.f)
  names(cols)<-paste0("y_",names(sectors.f))
  plot(pca, choix=c("ind"), label="none", col.ind=cols[names(kcl$cluster)],cex=3)
  #pca$ind[,c("Dim.1","Dim.2")]
  legend('topleft', cex=1.,  legend =paste("Region",levels(sectors.f)) , fill = 1:nlevels(sectors.f), merge = F, bty = 'n')
  legend('bottomright', cex=1.,  legend =paste(" Regions for",main))
  }

  numC<-3
  fit <- pca(na.omit(newdata), scale = "uv", nPcs =numC, center = TRUE)

   plotPcs(fit, pcs = 1:nP(fit), type = c("scores", "loadings"),
           sl = NULL, hotelling = 0.5,col=kcl$cluster,pch=15,cex=1.5)   # hotelling = 0.95 pca methods?

   cat("\n",main,"\n")
  print(summary(fit) )# print variance accounted for

   }
  if(f_4) return(kcl$cluster) else return(fmm$classification)
}
