transformE <-
function(d, Ave=TRUE,cAve=FALSE,Zero=FALSE,zero=NULL,diagonal=TRUE,imageplot=FALSE,old=NULL){
d<-as.matrix(d,nrow=nrow(d),ncol=ncol(d))
if(nrow(d)<4 || ncol(d)<3)return()
if(sd(d,na.rm=TRUE)<1e-8)return()
  if(Zero){
    if(is.null(zero)) zero<-mean(d,na.rm=TRUE)
    d<-d-zero
    cat("\n zero set to ",zero)
  } else zero=0
  if(Ave)smat<-addave(d,cAve=cAve) else smat<-d
  cat("\n DATA SET HAS ",nrow(d), ncol(d) , " Rows and Columns\n")
  if(imageplot)imagenan(smat,main=paste0("Input Data Set"))
  m<-corre(smat)     #smat is data set space x time
       #result is matrices of space : s=slope ,sse=std error,
                                                  #b=intercept, bse=std error, r2=r^2, N=points, pslope=prob no corr.,
                                                  # node=if data has little variation or too few points
                                                  #p_par=prob for slope=1  (<0.05 indicates NOT parallel)
  m$zero=zero

  #imagenan(m$b,zlim=c(-4,30),main=" Transform after corr: Intercept",xlab="Row", ylab="Col")
  Tx<-transf(smat,m, equita=FALSE,diagonal=diagonal)   #make transform based on least squared fits (m$s is least squred slope)
  if(imageplot)imagenan(Tx$x,main=paste0("Least Squared Transform"))

  E<-equitable(m)
                       # E  s=slope       numrun=# of iterations
                                         #  rtestxm=implied mean r^2   rtestxsd=error in
                                         # s1=sloppe matrix after on iteration       sd1=std dev for each slope from averaging approximations

  m$p_par<-parallel_Etest(Es=E$s,p=m$pslope,p_par= m$p_par)  #readjust parallel prob based on very good equitable fits
  #imagenan(m$p_par,zlim=c(0.01,0.05),outside.above.color='red',main="Etransform :Parallel=red")
  Em<-m                #m contains the least squared correlation info
  Em$E.s<-E$s
  if(!is.null(old)){
    Em$E.s[is.na(old$ET.Es )]<-NA
  }
  #image(E$sd1,main="Transform: sd1")
  Em$E.sd1<-E$sd1
  #image(Em$E.sd1,main="Transform: Em$E.sd1")
  Em$E.snode<-E$snode
  #Em$s<-E$s            #add slope matrix with equitable version

  #imagenan(Em$b,zlim=c(-4,30),main=" Transform before equitableb: Intercept",xlab="Row", ylab="Col")
                      #replace intercept matrix with equitable version
  Eb<-equitableb(Em)   # make equitable intercepts   using Em$s containg equitable slope matrix

  Em$E.b<-Eb$b            #replace old intercepts with equitable ones
  #image(Em$E.sd1,main="Transform 3: Em$E.sd1")

  ET<-transf(smat,Em,equita=TRUE,diagonal=diagonal)   #minfinal set slope that 1/s calcs are ignored
  if(imageplot)imagenan(ET$x,main=paste0("Equitable Transform"))
    #transf(smat,Em)   #make Equitable transform  ET$x and std dev in ET$xsd


  names(m)<-paste0("l.s.",names(m))
  names(Tx)<-paste0("l.s.",names(Tx))
  names(E)<-paste0("E.",names(E))
  names(Eb)<-paste0("E.",names(Eb))
  names(ET)<-paste0("ET.",names(ET))

  cat("\nUsing average as Reference to make transform")
  ETave<-transave(smat,Em,equita=TRUE)
#  image(ETave$Ave.ET.x,main=paste0(" Average used as Reference to\nform Equitable Transform"))


  smat<-list(smat=smat)
  TransE<- c(smat,m,Tx,E,Eb,ET,ETave)
  if(Zero){   #if Zero set then either none or all " data sets" should be modified
    TransE$smat<-TransE$smat+zero
    TransE$ET.x<-TransE$ET.x+zero
    TransE$l.s.x<-TransE$l.s.x+zero
    TransE$Ave.ET.x<-TransE$Ave.ET.x+zero
  }

  return(TransE)
}
