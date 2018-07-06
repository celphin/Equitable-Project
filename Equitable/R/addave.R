addave <-
function(d,cAve=FALSE){     #add  averages over columns (applied for each row) d[r,] and apend onto dataset
  newd<-d
  newd<-cbind(d,rowMeans(d, na.rm=TRUE))
  newd[is.nan(newd)]<-NA
  if(cAve){
    newd<-rbind(newd,colMeans(newd, na.rm=TRUE))
    rownames(newd)[nrow(newd)]<-paste0("Col_Ave")
  }
 colnames(newd)[ncol(newd)]<-paste0("Row_Ave")
        return(newd)
}
