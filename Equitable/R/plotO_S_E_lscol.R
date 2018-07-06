plotO_S_E_lscol <-
function(orig,E,ls,x_unit=NULL,y_unit=NULL,
                          signal=NULL,Eave,ylimits=NULL,xlimits=NULL,genname=NULL, of=FALSE,lf=FALSE,ef=TRUE,
                          avef=FALSE){
  if(is.null(genname))genname<-"Data"
if(is.null(ylimits)) ylimits<-c(min(E,na.rm=TRUE),max(E,na.rm=TRUE))
if(!is.infinite(ylimits[1])){
if(is.null(xlimits)) xlimits<-c(1,nrow(E))

if(of)plot_columns(orig,main=paste(" Noise: Original",genname),xlimits=xlimits,limits=ylimits,x_unit=x_unit,y_unit=y_unit)
if(!is.null(signal))plot_columns(signal,main=paste("No Noise: Signal ",genname),xlimits=xlimits,limits=ylimits,x_unit=x_unit,y_unit=y_unit)
if(ef)plot_columns(E,main=paste("Noise: Equitable Transform",genname),xlimits=xlimits,limits=ylimits,x_unit=x_unit,y_unit=y_unit)
if(avef)plot_columns(Eave,main=paste("Noise: Equitable Transform REFERENCED",genname),xlimits=xlimits,limits=ylimits,x_unit=x_unit,y_unit=y_unit )
if(lf)plot_columns(ls,main=paste("Noise: Least Squared Transform",genname),xlimits=xlimits,limits=ylimits,x_unit=x_unit,y_unit=y_unit )
if(!is.null(signal))plot_columns(signal,main="No Noise: Signal",xlimits=xlimits,limits=ylimits,x_unit=x_unit,y_unit=y_unit)
} else cat("\nplotO_S_E_lscol : ERROR: transform is all Nan")
}
