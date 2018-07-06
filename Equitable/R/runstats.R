runstats <-
function(Tx){
  sumNA<-sum(length(which(is.na(Tx$smat))))
  Nt<-prod(dim(Tx$smat))
  fr<-sumNA/Nt
  cat("\ntotal of NA transformed data is ",sumNA," with total ",Nt," Fraction of data set that is NA is ",fr )
  fac<-sqrt(1-fr)
  newM<-nrow(Tx$smat)*fac
  newN<-ncol(Tx$smat)*fac
  sdfactornew<- sqrt(1/(newN-1)+1/(newM-1))
  sdfactor<- sqrt(1/(nrow(Tx$smat)-1)+1/(ncol(Tx$smat)-1))
  cat("\n  col= ",ncol(Tx$smat),"  row= ",nrow(Tx$smat), " initial scale factor= ",sdfactor)
  cat("\neffective  col= ",newN,"effective  row= ",newM, " scale factor= ",sdfactornew)

  cat("\noriginal data\n")
  nastat(Tx$smat)
  minmax(Tx$smat)
  cat("Least squared transform \n")
  nastat(Tx$l.s.x)
  minmax(Tx$l.s.x)
  cat("Equitable transform \n")
  nastat(Tx$ET.x)
  minmax(Tx$ET.x)
  cat("Least squared transform -Original data\n")
  nastat(Tx$l.s.x-Tx$smat)
  cat("Equitable transform -Original data\n")
  residE<-nastat(Tx$ET.x-Tx$smat)

  cat("Equitable transform using ref column (Ave usually) -Original data\n")
  nastat(Tx$Ave.ET.x-Tx$smat)                    #ETave$Ave.ET.x

  cat("\n")
  # cat("Equitable transform Errors\n")
  # nastat(Tx$ET.xsd)
  return(residE)
}
