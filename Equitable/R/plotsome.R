plotsome <-
function(T,num=5,signal=NULL,limits=NULL,xlimits=NULL,
                   indiv=FALSE,columns=FALSE,images=TRUE,density=FALSE,versus=FALSE,xvsref=NULL,yline=3,yma=5,
                   transpose=FALSE,of=FALSE,lf=FALSE,ef=TRUE,avef=FALSE,errb=FALSE,row_unit=NULL,col_unit=NULL,z_unit=NULL,genname=NULL,
                   stderror=FALSE,fcontour=TRUE){
  if(is.null(z_unit))z_unit<-"Data Value"
  if(transpose){
    o<-t(T$smat); E<-t(T$ET.x);  ls<-t(T$l.s.x );As<-t(T$l.s.x ); if(!is.null(signal))signal<-t(signal); Eave<-t(T$Ave.ET.x)
    Eerr<-t(T$ET.xsd); lserr<-t(T$l.s.xsd ) ; Eaveerr<-t(T$Ave.ET.xsd) ; genname<-paste0("Transposed ",genname)
    if(!is.null(col_unit))y_unit<-col_unit else y_unit<-"COLUMNS"
    if(!is.null(row_unit))x_unit<-row_unit else x_unit<-"ROWS"
    if(stderror){
      Eerr<- Eerr/sqrt(t(T$ET.EN)-1) ; lserr<-lserr/sqrt(t(T$l.s.EN)-1); Eaveerr<-Eaveerr/sqrt(t(T$Ave.ET.EN)-1)
      genname<-paste0(genname," std. error of Mean")
    }

  } else {
    o<-T$smat; E<-T$ET.x;  ls<-T$l.s.x ;signal<-signal ;Eave<-T$Ave.ET.x
    Eerr<-T$ET.xsd; lserr<-T$l.s.xsd  ; Eaveerr<-T$Ave.ET.xsd
    if(!is.null(col_unit))x_unit<-col_unit else x_unit<-"COLUMNS"
    if(!is.null(row_unit))y_unit<-row_unit else y_unit<-"ROWS"


    #if(is.null(genname))genname<-" Data"

    if(stderror){
      Eerr<- Eerr/sqrt((T$ET.EN)-1) ; lserr<-lserr/sqrt((T$l.s.EN)-1); Eaveerr<-Eaveerr/sqrt((T$Ave.ET.EN)-1)
      genname<-paste0(genname," std. error of Mean")
    }
  }
                                                   #xvsref contains ref column to plot against
  if(images)plotimages(o,E,ls,signal,Eave,genname=genname,of=of,lf=lf,
                       ef=ef,avef=avef,zlimits=limits,row_unit=y_unit,col_unit=x_unit,yma=yma,yline=yline,fcontour=fcontour)

  if(indiv)plotindivid(o,E,ls,num=num,signal,Eave,genname=genname,of=of,lf=lf,ef=ef,avef=avef,err=errb,
                       Eerr=Eerr, lserr=lserr, Eaveerr=Eaveerr,ylimits=limits,xlimits=xlimits,
                       x_unit=y_unit,y_unit=z_unit)
  if(columns)plotO_S_E_lscol(o,E,ls,signal,Eave,genname=genname,of=of,lf=lf,ef=ef,avef=avef,ylimits=limits,xlimits=xlimits,
                             x_unit=y_unit,y_unit=z_unit)
  if(versus)plotversus(o,E,ls,signal,Eave,genname=genname,of=of,lf=lf,ef=ef,avef=avef,ylimits=limits)
  if(!is.null(xvsref)){
    if(xvsref=="max"||length(which(!is.na(Eave[,xvsref])))==0 )xvsref<-floor(which(abs(Eave)==max(abs(Eave),na.rm=TRUE))/nrow(Eave))
    plot_vsref(Eave,xvsref,
                       main=paste("Equitable Transform\n compared to Column  (Black)",xvsref),
                       lty="l",limits=limits)
  }
}
