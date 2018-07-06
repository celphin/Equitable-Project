newxfac <-
function(dissimilarity=Td6_9$ET.x,sname,dendfactor=NULL){

  xfac<-allfactors(sname)



  xf<-NULL
  for(jp in 1:ncol(dissimilarity))xf<-c(xf,which(colnames(dissimilarity)[jp]==rownames(xfac)))
  xf  #length(xf)
  xfac<-xfac[xf,]
  rownames(xfac)

  k<-which(colnames(dissimilarity)=="Row_Ave")  #k<-which(colnames(s)[4:10]=="Row_Ave")
  if(length(k)>0){
    xfac1<-xfac
    for (j in 1:length(k)){
      if(k[j]<=nrow(xfac)){
        xfac1<-rbind(xfac[1:(k[j]-1),],rep(NA,ncol(xfac)),xfac[k[j]:(nrow(xfac)),] )
      } else {   #if rowave is lastcolumn
        xfac1<-rbind(xfac[1:(k[j]-1),],rep(NA,ncol(xfac)))

      }
      #add a row for each row_ave that occurs
      #xfac<-rbind(xfac,rep(NA,ncol(xfac)))
      rnn<-paste0("Row_Ave",j)
      rownames(xfac1)[k[j]]<-rnn
      alist<-c("Id","Plot","Year","Plant.Plot.ID","Plant.Field.ID","Snow","Average.Temp","June.Temp","July.Temp","August.Temp")
      for ( jj in alist) xfac1[rnn,jj]<-round(mean(xfac[,jj],na.rm=TRUE),digits=1)
      colnames(dissimilarity)[k[j]]<-paste0("Row_Ave",j)
      xfac<-xfac1
    }

  }
  if(!is.null(dendfactor)){  #id endfactor is defined then tag it on asn additional column to xfac
    xfac<-cbind(xfac,dendfactor)
    colnames(xfac)[ncol(xfac)]<-"Dendo_branches"
  }
  #cat("\n xfac ",rownames(xfac))
  #cat("\n dissim ",colnames(dissimilarity))
  cat("\n\n\nlength of xfac",nrow(xfac),"number of columns dissimilarity ",ncol(dissimilarity))
  return(xfac)
}
