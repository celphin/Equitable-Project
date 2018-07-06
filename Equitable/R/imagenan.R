imagenan <-
function(x,yline=3,yma=5,
                      col = topo.colors(255),outside.below.color='black',outside.above.color='white',na.color='gray',
                      ...){
  x<-as.matrix(x)
  .pardefault <-par(no.readonly = TRUE)
  #x[1:10,5:10]<-NA
  reverse <- nrow(x) : 1
  x <- x[reverse,]
  #rownames(x)<-rownames(x)[reverse]
  zlim=c(min(x,na.rm=TRUE),max(x,na.rm=TRUE))
  if(zlim[2]<= zlim[1]+1e-10)zlim<-c(zlim[1],zlim[1]+1/100)  #  zlim[2]<= zlim[1]+1e-10
  if(!is.null(rownames(x)))rLabels <- rownames(x) else {rLabels<-c(reverse); rownames(x)<-c(reverse)}
  cLabels <- colnames(x)
  er<-nrow(x)
  ec<-ncol(x)
  main <-" "
  row_unit <-"Rows"
  col_unit<-"Columns"
  zunit<-"Intensity"
  rtick=1          # for plotting use start at 30 days for xtick marks
  ctick=1
  rtickinc=round(er/10)    # for plotting use every 1 unit for "space" tick marks
  ctickinc=round(ec/10)    # for plotting use every 1 unit for "space" tick marks
  if(rtickinc==0)rtickinc=1
  if(ctickinc==0)ctickinc=1

  rnames<-rownames(x)
  cnames<-colnames(x)
  attr=c(seq(rtick,er,by= rtickinc))
  attc=c(seq(ctick,ec,by= ctickinc))
  rlbls=rnames[seq(rtick,er,by= rtickinc)]
    clbls=cnames[seq(ctick,ec,by=ctickinc)]
  # check for additional function arguments
  if( length(list(...)) ){
    Lst <- list(...)
    if( !is.null(Lst$zlim) ){
      zlim<-c(Lst$zlim)
      if(zlim[2]<= zlim[1])zlim<-c(zlim[1],zlim[1]+1/100)
      # min <- Lst$zlim[1]
      # max <- Lst$zlim[2]
    }
    if( !is.null(Lst$yLabels) ){
      cLabels <- c(Lst$cLabels)
    }
    if( !is.null(Lst$xLabels) ){
      rLabels <- c(Lst$rLabels)
    }
    if( !is.null(Lst$main) ){
      main <- Lst$main
    }
    if( !is.null(Lst$row_unit) ){
    row_unit <-  Lst$row_unit
    }
    if( !is.null(Lst$col_unit) ){
      col_unit <-  Lst$col_unit
    }
    if( !is.null(Lst$rlbls) ){
      rlbls <-  Lst$rlbls
    }
    if( !is.null(Lst$clbls) ){
      clbls <-  Lst$clbls
    }
    if( !is.null(Lst$zunit) ){
      zunit <-  Lst$zunit
    }


  }

  # check for null values
  if( is.null(rLabels) ){
    #rLabels <- c(1:nrow(x))
    rLabels <- c(reverse)
  }
  if( is.null(cLabels) ){
    cLabels <- c(1:ncol(x))
  }

  #layout(matrix(data=c(1,1,2,3), nrow=2, ncol=2), widths=c(4,1), heights=c(1,1))
  layout(matrix(data=c(1,1,2,3), nrow=2, ncol=2), widths=c(5,1.25), heights=c(1,0.5))

  # Red and green range from 0 to 1 while Blue ranges from 1 to 0
  #   ColorRamp <- rgb( seq(0,1,length=256),  # Red
  #                     seq(0,1,length=256),  # Green
  #                     seq(1,0,length=256))  # Blue
  #  ColorLevels <- seq(min, max, length=length(ColorRamp))
  ##ColorRamp <- matlab.like2(100) #temperature

  zstep <- (zlim[2] - zlim[1]) / length(col); # step in the color palette
  newz.below.outside <- zlim[1] - 2 * zstep # new z for values below zlim
  newz.above.outside <- zlim[2] + zstep # new z for values above zlim
  newz.na <- zlim[2] + 2 * zstep # new z for NA

  x[which(x<zlim[1])] <- newz.below.outside # we affect newz.below.outside
  x[which(x>zlim[2])] <- newz.above.outside # we affect newz.above.outside
  x[which(is.na(x>zlim[2]))] <- newz.na # same for newz.na

  zlim[1] <- zlim[1] - 2 * zstep # extend lower limit to include below value
  zlim[2] <- zlim[2] + 2 * zstep # extend top limit to include the two new values above and na

  col <- c(outside.below.color, col[1], col, outside.above.color, na.color) #correct by including col[1] at bottom of range
  #cat(zlim)
  ColorRamp <-col
  # ColorRamp <- rainbow(100) #snow
  if(zlim[1]>=zlim[2] )zlim[2]<-zlim[1]+0.01*zlim[1]
 ColorLevels <- seq(zlim[1], zlim[2], length=100)

  # Reverse Y axis (rows)

 # rLabels <- rLabels[reverse]


  # Data Map
  #par(mar = c(3,5,2.5,2))
  par(mar =  c(5,4,4,2))
  par(mar =  c(6,yma,4,2))
  #set ylab=""
  #mtext("Total Milk Production (in pounds?)",side=2,line=5)
            #  b l t r margins c(5,6,4,2)+0.1)  mgp (axis title,label,line   par(mar=c(5,6,4,2)+0.1,mgp=c(5,1,0))default 3,1,0
  #cat("\ncol row ",col_unit,row_unit)
  image(1:length(cLabels),1:length(rLabels),  t(x), col=ColorRamp, xlab=col_unit,main=main,
        ylab="", axes=FALSE, zlim=zlim)
  mtext(row_unit,side=2,line=yline)
  if( !is.null(main) ){
    title(main=main)
  }
  axis(BELOW<-1, at=attc, labels=clbls, cex.axis=0.7)
  # axis(BELOW<-1, at=1:length(xlbls), labels=xlbls, cex.axis=0.7)
  #  axis(LEFT <-2, at=1:length(ylbls), labels=ylbls, las= HORIZONTAL<-1,
  #       cex.axis=0.7)
  axis(LEFT <-2, at=attr, labels=rlbls, las= HORIZONTAL<-1,
       cex.axis=0.7)

  # Color Scale
  par(mar = c(3,1,2.5,4))
  image(1, ColorLevels,
        matrix(data=ColorLevels[1:length(ColorLevels)-1], ncol=length(ColorLevels)-1),
        col=ColorRamp,
        xlab="",ylab=zunit,
        xaxt="n")


  par(.pardefault)
  layout(1)
  par(mfrow=c(1,1))

}
