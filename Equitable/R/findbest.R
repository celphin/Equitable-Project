findbest <-
function(val,comparelist,ir){
  bestintersectr<-NULL
  if(!is.nan(val)){
    for(r in 1:(length(ir)-1)){
      dist<-abs(val-comparelist[ir[r]])
      disttot<-abs(comparelist[ir[r+1]]-comparelist[ir[r]])
      if(dist<disttot && val>=comparelist[ir[r]] ){if(dist<disttot/2)bestintersectr<-(r) else bestintersectr<-(r+1)}
    }
    if(is.null(bestintersectr)){
      if(val<comparelist[ir[1]])bestintersectr<-1 else bestintersectr<-length(ir)
    }
  }
  return(bestintersectr)
}
