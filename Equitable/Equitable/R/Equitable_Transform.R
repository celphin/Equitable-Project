#
# library("Hmisc")
# library("SDMTools")
# library("aplpack")   #for bagplot


#' Plots False Colour Image
#'
#' Displays a false colour image of two dimensional data set
#'
#' @param x 2D data to display
#' @param yline Number of lines in (left side) to where image begins
#' @param yma Number of characters in (left side)
#' @param col colour table
#' @param outside.below.color Colour below threshold
#' @param outside.above.color Colour above threshold
#' @param na.color Colour of NAs (see \code{\link[graphics]{par}})
#' @param ... other plot parameters eg zlim list with min and max range of color table data e.g. zlim=c(2,5)
#'
#' @return None
#'
#' @examples
#' d<-eg4()
#' imagenan(d)
#' imagenan(d,zlim=c(2,8))
#' imagenan(d,zlim=c(3,8),outside.above.color='red',outside.below.color='tan')
#' d=data.frame(cbind(c(1:4),c(2,5,NA,NA) ))
#' imagenan(d)
#' @export
imagenan <- function(x,yline=3,yma=5,
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
  cex.main=0.9
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
    if( !is.null(Lst$cex.main) ){
      cex.main <- Lst$cex.main
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
        ylab="", axes=FALSE, zlim=zlim,cex.main=cex.main)
  mtext(row_unit,side=2,line=yline)
  if( !is.null(main) ){
    title(main=main,cex.main=cex.main)
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

corre<- function(matr){
  nc<-ncol(matr)
    val<-sapply(1:nc,FUN=runy,mat=matr)
    val1<-array(as.numeric(val),dim=c(nc,9,nc)); colnames(val)<-colnames(matr)
    m<-list(
    s=t(val1[,1,]),
    sse=t(val1[,2,]),
    b=t(val1[,3,]),
    bse=t(val1[,4,]),
    r2=t(val1[,5,]),
    N=t(val1[,6,]),
    pslope=t(val1[,7,]),
    node=t(val1[,8,]),
    p_par=t(val1[,9,])
   )
    m$pslope[which(is.nan(m$pslope))]<-NA
    if(length(colnames(matr))!=0) rown<-paste0("y_",colnames(matr)) else rown<-colnames(matr)
    coln<-colnames(matr)
    rownames(m$s)<-rownames(m$sse)<-rownames(m$b)<-rownames(m$bse)<-rownames(m$r2)<-rownames(m$N)<-rownames(m$pslope)<-rownames(m$node)<-rownames(m$p_par)<-rown
    colnames(m$s)<-colnames(m$sse)<-colnames(m$b)<-colnames(m$bse)<-colnames(m$r2)<-colnames(m$N)<-colnames(m$pslope)<-colnames(m$node)<-colnames(m$p_par)<-coln
  return(m)
} #correlations of all possible pairs of columns from matr: output is list of matrices of slopes, intercepts, errors, etc

runy<-function(y,mat) {
      valy<-sapply(1:ncol(mat),FUN= liner,y=y,mat=mat)
      v<-as.list(t(valy))
      return(v)
} # applies linear fits fitting column y to every column in mat as the independent variable (x)

liner<-function(x,y,mat,minnum=4,minsd=10e-10) {
  fy<-mat[,y] ; fx<-mat[,x]
  ncomp<-length(which(complete.cases(fy,fx)))
  if(ncomp>=minnum && abs(sd(fx, na.rm=TRUE))>minsd){
    fit<-lm(fy~fx, na.action=na.exclude )
    fit_coef<-coef(summary(fit))
    if(dim(fit_coef)==2) {
      p_par<-parallel_test(fx,fy)
    param<-list(
      s=fit_coef[2,"Estimate"],sse=fit_coef[2,"Std. Error"],
      b=fit_coef["(Intercept)","Estimate"], bse=fit_coef["(Intercept)","Std. Error"],
      r2=summary(fit)$r.squared,
      N=summary(fit)$df[2]+2,
      pslope=fit_coef[2,"Pr(>|t|)"],
      node=0,
      p_par=p_par
    )

    } else{
      param<-list(
        s=NA,sse=NA,
        b=NA, bse=NA,
        r2=NA,
        N=NA,
        pslope=NA,
        node=1,
        p_par=NA
      )
    }
  } else {
    param<-list(s=NA,sse=NA, b=NA, bse=NA,r2=NA, N=ncomp, pslope=NA,node=1,p_par=NA)
  }
  return(param)
}   #linear fits y=ax+b returns a list of parameters for the fit


simpletran<-function(I,mat) {      #mu are time row averages fro each column (space)
  s<-mat$Es;
  b<-mat$Eb;
  p<-mat$Ep

  zw<-matrix(NA,nrow=nrow(I),ncol=ncol(I))
  zsd<-matrix(NA,nrow=nrow(I),ncol=ncol(I))
  EN<-matrix(NA,nrow=nrow(I),ncol=ncol(I))
  for(y in 1:ncol(I)){
    for(t in 1:nrow(I)){
      zw[t,y]<-weighted.mean(s[y,]*I[t,]+b[y,],(1-p[y,])^2/sum((1-p[y,])^2, na.rm=TRUE) , na.rm=TRUE)
      zsd[t,y]<-sqrt(sum((1-p[y,])^2/sum((1-p[y,])^2, na.rm=TRUE) * (s[y,]*I[t,]+b[y,] - zw[t,y])^2,na.rm = TRUE ))
      EN[t,y]<-length(which(complete.cases(s[y,],b[y,])))
    }
  }

  cat("\n total of NA transformed data is ", sum(length(which(is.na(I)))))
  #image(zw)
  zw[which(is.na(zw))]<-I[which(is.na(zw))]   #copy dec 5 830am
  #image(zw)

  Tx<-list(
    x=zw,
    xsd=zsd,
    EN=EN,
    Es=s,
    Eb=b,
    Ep=p
  )

  return(Tx)
} # Creates a transform based on the dataset I and matrix information of already masked matrices (Es,Eb,Ep)

   transf<-function(I,mat,minp=0.5,equita=FALSE,diagonal=TRUE) {      #mu are time row averages fro each column (space)

    if(!equita){ s<-mat$s; b<-mat$b; node<-mat$node; se<-mat$sse }
    else {s<-mat$E.s; b<-mat$E.b; node<-mat$E.snode; se<-mat$E.sd1  #se<-mat$sse
    }

    ls<-mat$s
    lse<-mat$sse
    lb<-mat$b

    p<-mat$pslope

    # mu<-colMeans(I,na.rm = TRUE)

    #ls[p>minp]<-NA
    for (r in 1:nrow(ls))ls[abs(ls[r,])< abs(ls[,r]) & (abs(ls[r,])-lse[r,])< 0,r]<-NA
    s[is.na(ls)]=NA
    b[is.na(ls)]=NA

    frac<-sum(length(which(is.na(I))))/prod(dim(I))
    cat("\nFraction of data array that is missing is", frac)
    if(frac< 1/2)maxprob<- 2/3 else  maxprob<- 0.9
    p[is.nan(p)]<-NA
    if(length(p[!is.na(p)])>2){
    if(mean(1-p,na.rm=TRUE)==1)maxprob<- 1}
    cat("\nMaxprob set to : ",maxprob)
    for (r in 1:nrow(s))s[abs(s[r,])< abs(s[,r]) & (abs(s[r,])-lse[r,])< 0,r]<-NA
    b[is.na(s)]=NA

    for (r in 1:nrow(s)){
      crit=1
      qr<-quantile(abs(s[r,]),prob=maxprob,na.rm = TRUE)      #2/3 works welll Dec 10 740pm
      if(!is.na(qr) && qr>crit)crit=qr
      s[r,abs(s[r,])> abs(s[,r]) & abs(s[r,])> crit]<-NA   #crit is larger of 1 and quantile85
    }


    b[is.na(s)]=NA
    cat("\nDiagonal= ",diagonal)
    if(!diagonal){
      diag(s) <- NA
      diag(b) <- NA
    }


    zw<-matrix(NA,nrow=nrow(I),ncol=ncol(I))
    zsd<-matrix(NA,nrow=nrow(I),ncol=ncol(I))
    EN<-matrix(NA,nrow=nrow(I),ncol=ncol(I))
    for(y in 1:ncol(I)){
      for(t in 1:nrow(I)){

        zw[t,y]<-weighted.mean(s[y,]*I[t,]+b[y,],(1-p[y,])^2/sum((1-p[y,])^2, na.rm=TRUE) , na.rm=TRUE)
        zsd[t,y]<-sqrt(sum((1-p[y,])^2/sum((1-p[y,])^2, na.rm=TRUE) * (s[y,]*I[t,]+b[y,] - zw[t,y])^2,na.rm = TRUE ))
        EN[t,y]<-length(which(complete.cases(s[y,],b[y,])))
      }
      #zw[is.na(zw[,y]),y]<-mu[y]   #post dec 5 830AM
    }
    zw[is.nan(zw)]<-NA
    zsd[is.nan(zsd)]<-NA

   cat("\nequita=", equita," total of NA transformed data is ", sum(length(which(is.na(I)))))
   #image(zw)
   if(diagonal) zw[which(is.na(zw))]<-I[which(is.na(zw))]   #copy dec 5 830am
    #image(zw)
    colnames(zw)<-colnames(I) ; rownames(zw)<-rownames(I)
    colnames(zsd)<-colnames(I) ; rownames(zsd)<-rownames(I)
    colnames(EN)<-colnames(I) ; rownames(EN)<-rownames(I)

    Tx<-list(
      x=zw,
      xsd=zsd,
      EN=EN,
      Es=s,
      Eb=b,
      Ep=p
    )

    return(Tx)
  } # Creates a transform based on the dataset I and matrix information mat


  reduce_s<-function(s,se,p,ls,lse ,node,minp=0.5,mins=0,b=FALSE){

    blankthreshold<-0.5
    s1<-s
    s1[(p>minp | (abs(s1)-se)<mins)]<-NA
    ls[(p>minp | (abs(ls)-lse)<mins)]<-NA
    sp<-s1

    s1[which(is.na(t(s1)))]<-NA
    ls[which(is.na(t(ls)))]<-NA

    blankcols<-apply(s1, 1,FUN=function(x) sum(is.na(x)))/ncol(s1)
    if(b){        #this is fundamental to correct functioning of transform
      s1[which(blankcols>blankthreshold),]<-ls[which(blankcols>blankthreshold),]     #these rows have almost all slopes of zero so set them to zero
      node[which(blankcols>blankthreshold),]<-2    #need to reset intercept as well
      s1[,which(blankcols>blankthreshold)]<-NA   #ignore infinite slopes
    } else {
      s1[which(blankcols>blankthreshold),]<-NA
   #   imagenan(s1,zlim=c(0,4),main=" Final:Remove full columns",xlab="Row", ylab="Col")
      #image(s1,zlim=c(0,row.3rd[nrow(s)]+1),main="Final: Remove transpose elements",xlab="Row", ylab="Col")
      s1[,which(blankcols>blankthreshold)]<-NA
    }
    s1[row(s1)==col(s1)]<-1
   # imagenan(s1,zlim=c(0,4),main=" Final:Inside Remove transpose rows",xlab="Row", ylab="Col")

     s1andnode<-list(s1=s1,node=node)
    return(s1andnode)

  } #blank out poor rows and columnsin slope matrix to create equitable matrix

  equitable<-function(mat){
    cat("\n Average 1-p : ",mean(1-mat$pslope,na.rm=TRUE), "\n")  #mean(1-Td96$l.s.pslope,na.rm=TRUE) mean(1-c(Td96$l.s.pslope),na.rm=TRUE)
    nc<-ncol(mat$s)
    s1<-mat$s
    rtest<-testE(s1)        #test corrected matrix for equitability
    cat("Equitable: Average R^2 test ", rtest$xm," Average std dev of R^2 ",rtest$xsd," \n")
    s1andnode <-reduce_s(s1,mat$sse,mat$pslope,mat$s,mat$sse,mat$node)
      s1<-s1andnode$s1
    node<-s1andnode$node
    mat$b<-s1andnode$b1

    rtest<-testE(s1)        #test corrected matrix for equitability
    cat("Equitable: Before iteration (Reduced slopes) Average R^2 test ", rtest$xm," Average std dev of R^2 ",rtest$xsd," \n\n")
    #first run calc sd for each slope as well
    Es<-outer(1:nc,1:nc,Vectorize(Eslope),
              s=as.data.frame(c(s1)),nc=nc,p=as.data.frame(c(mat$pslope)))  #input matrices must be as data.frames output must be same length
    colnames(Es)<-colnames(mat$s) ; rownames(Es)<-rownames(mat$s)
    E1s<-Es
    E1sd<-outer(1:nc,1:nc,Vectorize(Esd),
               s=as.data.frame(c(s1)),nc=nc,p=as.data.frame(c(mat$pslope)))  # first iteration find errors for each slope
    colnames(E1sd)<-colnames(mat$s) ; rownames(E1sd)<-rownames(mat$s)
    E1sN<-outer(1:nc,1:nc,Vectorize(sN),
                s=as.data.frame(c(s1)),nc=nc,p=as.data.frame(c(mat$pslope)))  # first iteration find errors for each slope
    colnames(E1sN)<-colnames(mat$s) ; rownames(E1sN)<-rownames(mat$s)

    rtest<-testE(Es)
   numrun=1
   # r2max=0.001, semax=0.01, maxrun=8
    E<-list(
      s=Es,
      numrun=numrun,
      rtestxm=rtest$xm ,     #xm and xsd
      rtestxsd=rtest$xsd,
      s1=E1s,     #first iteration matrix
      sd1=E1sd,     #first iteration matrix sd dev error for each slope
      sN=E1sN,
      snode=node

    )
    E<-equit(E,mat$pslope)  # E is list and Es is latest slope matrix

    #after finishing put por slopes back in
    #imagenan(E$s,zlim=c(0,4),main=" Equitable:Pre pre Final",xlab="Row", ylab="Col")
    E$s[which(is.na(E$s))]<-mat$s[which(is.na(E$s))]

    E$s1[which(is.na(E$s1))]<-mat$s[which(is.na(E$s1))]
    E$sd1[which(is.na(E$s1))]<-mat$sse[which(is.na(E$s1))]
    #imagenan(E$s,zlim=c(0,4),main=" Equitable:Pre Final",xlab="Row", ylab="Col")
    #s1andnode<-reduce_s(E$s,E$sd1,mat$pslope,mat$s,mat$sse,mat$node,minp,mins,sdfactor,asigfactor,b=TRUE)   #rerun setting some slopes to 1e-10 and their transpose to NA
    er<-matrix(0,nrow=nrow(E$s),ncol=ncol(E$s))
    s1andnode<-reduce_s(E$s,mat$sse,mat$pslope,mat$s,mat$sse,mat$node,b=TRUE)   #rerun setting some slopes to 1e-10 and
    E$s<-s1andnode$s1
    E$snode<-s1andnode$node

    #imagenan(E$s,zlim=c(0,4),main=" Equitable:Final",xlab="Row", ylab="Col")

    return(E)
  }  # calculates equitable matrix from information in mat. returns a list of matrices related to the equitable matrix constrcution

     equit<-function(E,p, r2max=0.001, semax=0.01, maxrun=8){
      nc<-ncol( E$s)
     cat("Slope: iteration ", E$numrun," Average R^2 test ", E$rtestxm," Average std dev of R^2 ",E$rtestxsd," \n")
     if(is.na(E$rtestxsd)) {E$rtestxsd<-0; cat("\nRtest std reset to 0")}
    if((abs(E$rtestxm-1)< r2max && E$rtestxsd< semax) || E$numrun>=maxrun){
    return(E)
    } else{     #if not good enough redo with new matrix Es
      E$numrun<-E$numrun+1    #run again

      Es<-outer(1:nc,1:nc,Vectorize(Eslope),
                s=as.data.frame(c(E$s)),nc=nc,p=as.data.frame(c(p)))  #input matrices must be as data.frames output must be same length
      colnames(Es)<-colnames(E$s) ; rownames(Es)<-rownames(E$s); E$s<-Es
      rtest<-testE(Es)
      E$rtestxm<-rtest$xm; E$rtestxsd<-rtest$xsd; E$s<-Es
      E<-equit(E,p)

                 #test new matrix for equitability  rtest$xm=mean r^2 and rtest$xsd =std dev
    }

    }  #recursive function to get convergence of slope matrix

     equitb<-function(E,Eb,s1,p, maxzero_std=0.1, zero=0.001, maxrun=3){   #numrun from 8 down to 3 &std from 0.015 to 0.1 (Jan 12 2018)
       nc<-ncol( E$b)
       cat("Intercept: ",E$numrun,E$rtestbm,E$rtestbsd,"\n")
      if ((abs(E$rtestbm) < zero  && E$rtestbsd < maxzero_std ) ||  E$numrun >=  maxrun) {  #return condition met
         return(E)
       } else{     #if not good enough redo with new matrix Es
         E$numrun<-E$numrun+1    #run again
         Eb<-outer(1:nc,1:nc,Vectorize(Eintercept),b=as.data.frame(c(Eb)),
                   s=as.data.frame(c(s1)),nc=nc,p=as.data.frame(c(p)))  #input matrices must be as data.frames output must be same length

         colnames(Eb)<-colnames(s1) ; rownames(Eb)<-rownames(s1)
         rtest<-testEb(Eb,s1)
         E$rtestbm<-rtest$xm; E$rtestbsd<-rtest$xsd ; E$b<-Eb

         E<-equitb(E,Eb,s1,p)

         #test new matrix for equitability  rtest$xm=mean r^2 and rtest$xsd =std dev
       }

     } #recursive function to get convergence of intercept matrix

    testE<-function(s){      #test for equitable using property axy*ayx=1 if equitable
      rtest<-s*t(s)
      xm<-mean(c(rtest), na.rm = TRUE)
      xsd<-sd(c(rtest), na.rm = TRUE)
      t<-list(xm=xm, xsd=xsd)
      return(t)
    } # tests equitability of the slope matrix s

    testEb <- function(b,s,minval=1e-6){
      xm<-mean(abs(c(b)), na.rm = TRUE)


      if(abs(xm)>minval) rtest<-(b+s*t(b))/xm else rtest<-0*b
      xm<-mean(c(rtest), na.rm = TRUE)
      xsd<-sd(c(rtest), na.rm = TRUE)
      t<-list(xm=xm, xsd=xsd)
      return(t)
    } # tests equitability of the intercept matrix s


 Eslope<-function(y,x,s,nc,p){
   s<-matrix(s,nrow=nc,ncol=nc) ;p<-matrix(p,nrow=nc,ncol=nc)
   #print(s)
   sx_=s[x,]; sy_<- s[y,]; py_<-p[y,]; px_<-p[x,]
   j<-which(complete.cases(sx_,sy_))
   if(length(j)>0){
    xvect<-sy_[j]/sx_[j]
    wvect<-(1-py_[j])^2*(1-px_[j])^2
     xm<-weighted.mean(xvect, wvect, na.rm = TRUE)

   } else {
     xm<-s[y,x]
   }

   return(xm)                   #may not be able to get sd from this: run separately if neccessary?
 } # find approximation to equitable using vector of reference column slopes

 Esd<-function(y,x,s,nc,p){                                  # find approximation to equitable using vector of reference column slopes
   s<-matrix(s,nrow=nc,ncol=nc) ;p<-matrix(p,nrow=nc,ncol=nc)
   #print(s)
   sx_=s[x,]; sy_<- s[y,]; py_<-p[y,]; px_<-p[x,]
   j<-which(complete.cases(sx_,sy_))
   if(length(j)>0){
     xvect<-sy_[j]/sx_[j]
     wvect<-(1-py_[j])^2*(1-px_[j])^2
     xm<-weighted.mean(xvect, wvect, na.rm = TRUE)
     xsd<-sqrt(sum(wvect * (xvect - xm)^2,na.rm = TRUE ))

   } else {
     xm<-s[y,x]
     if(!is.na(xm)){ xsd<-0 }else {xsd<-NA }
   }
   return(xsd)                   #: run separately if neccessary?
 }    #find std dev associatd with each approximation to equitable using vector of reference column slopes
 sN<-function(y,x,s,nc,p){                                  # find approximation to equitable using vector of reference column slopes
   s<-matrix(s,nrow=nc,ncol=nc) ;p<-matrix(p,nrow=nc,ncol=nc)
   #print(s)
   sx_=s[x,]; sy_<- s[y,]; py_<-p[y,]; px_<-p[x,]
   j<-which(complete.cases(sx_,sy_))

   if(length(j)>0){
     N<-length(j)

   } else {
     xm<-s[y,x]
     if(!is.na(xm)){ N<-1 }else {N<-NA }
   }
   return(N)                   #: run separately if neccessary?
 }    #find N associatd with each approximation to equitable using vector of reference column slopes
 bN<-function(y,x,b,s,nc,p){                                  # find approximation to equitable using vector of reference column slopes
   b<-matrix(b,nrow=nc,ncol=nc) ;s<-matrix(s,nrow=nc,ncol=nc) ;p<-matrix(p,nrow=nc,ncol=nc)
   #print(b)
   bx_=b[x,]; by_<- b[y,];syx<-s[y,x]; py_<-p[y,]; px_<-p[x,]; pyx<-p[y,x]   #these run up Nov 30/16
   #b_x=b[,x]; by_<- b[y,];sy_<-s[y,]; py_<-p[y,]; p_x<-p[,x]
   j<-which(complete.cases(bx_,by_))    #these run up Nov 30/16
   #j<-which(complete.cases(b_x,by_,sy_))
   if(length(j)>0 && !is.na(syx)){  #this run to Nov 30/16
     N<-length(j)
   } else {
     xm<-b[y,x]
     if(!is.na(xm)) N<-1 else N<-NA
   }

   return(N)                   #may not be able to get sd from this: run separately if neccessary?
 }# find N of approximation to equitable intercept using vector of reference equitable slopes and intercepts

 Eintercept<-function(y,x,b,s,nc,p){                                  # find approximation to equitable using vector of reference column slopes
   b<-matrix(b,nrow=nc,ncol=nc) ;s<-matrix(s,nrow=nc,ncol=nc) ;p<-matrix(p,nrow=nc,ncol=nc)
   #print(b)
   bx_=b[x,]; by_<- b[y,];syx<-s[y,x]; py_<-p[y,]; px_<-p[x,]; pyx<-p[y,x]   #these run up Nov 30/16
   #b_x=b[,x]; by_<- b[y,];sy_<-s[y,]; py_<-p[y,]; p_x<-p[,x]
   j<-which(complete.cases(bx_,by_))    #these run up Nov 30/16
   #j<-which(complete.cases(b_x,by_,sy_))
   if(length(j)>0 && !is.na(syx)){  #this run to Nov 30/16
    # if(length(j)>0 ){
                                     # x[i] <- old_i[y,i]-s[y,u]*old_i[u,i]       #oldest ran this way until dec 24/15
                                     # wt[i] <- (1-ps[y,i])*(1-ps[u,i])*(1-ps[y,u])  #up to Jan 7 ps u
     xvect<- by_[j]-syx*bx_[j]
     wvect<-(1-py_[j])*(1-px_[j])^2*(1-pyx)^2   #these run up Nov 30/16
                                               # xvect<- by_[j]+sy_[j]*b_x[j]
                                               # wvect<-(1-py_[j])*(1-py_[j])^2*(1-p_x[j])^2
     xm<-weighted.mean(xvect, wvect, na.rm = TRUE)

   } else {
     xm<-b[y,x]
   }
   return(xm)                   #may not be able to get sd from this: run separately if neccessary?
 } # find approximation to equitable intercept using vector of reference equitable slopes and intercepts

 Esdintercept<-function(y,x,b,s,nc,p){                                  # find approximation to equitable using vector of reference column slopes
   b<-matrix(b,nrow=nc,ncol=nc) ;s<-matrix(s,nrow=nc,ncol=nc) ;p<-matrix(p,nrow=nc,ncol=nc)
   #print(b)
   bx_=b[x,]; by_<- b[y,];syx<-s[y,x]; py_<-p[y,]; px_<-p[x,]; pyx<-p[y,x]   #these run up Nov 30/16
   #b_x=b[,x]; by_<- b[y,];sy_<-s[y,]; py_<-p[y,]; p_x<-p[,x]
   j<-which(complete.cases(bx_,by_))    #these run up Nov 30/16
   #j<-which(complete.cases(b_x,by_,sy_))
   if(length(j)>0 && !is.na(syx)){  #this run to Nov 30/16
     # if(length(j)>0 ){
     # x[i] <- old_i[y,i]-s[y,u]*old_i[u,i]       #oldest ran this way until dec 24/15
     # wt[i] <- (1-ps[y,i])*(1-ps[u,i])*(1-ps[y,u])  #up to Jan 7 ps u
     xvect<- by_[j]-syx*bx_[j]
     wvect<-(1-py_[j])*(1-px_[j])^2*(1-pyx)^2   #these run up Nov 30/16
     # xvect<- by_[j]+sy_[j]*b_x[j]
     # wvect<-(1-py_[j])*(1-py_[j])^2*(1-p_x[j])^2
     xm<-weighted.mean(xvect, wvect, na.rm = TRUE)
     xsd<-sqrt(sum(wvect * (xvect - xm)^2,na.rm = TRUE ))
   } else {
     xm<-b[y,x]
     if(!is.na(xm)) xsd<-0 else xsd<-NA
   }

   return(xsd)                   #may not be able to get sd from this: run separately if neccessary?
 }# find std dev of approximation to equitable intercept using vector of reference equitable slopes and intercepts

equitableb<-function(mat){     #was 10e-6 and 1 better at 10e-2 and 2  try 10e-1 and 2 try 1/8 and 1

  nc<-ncol(mat$b)
  #only use values whose ?? and slope prob<0.5
  s1<-mat$E.s    #this uses equitable values for slopes and reduce uses this slope-sse as check
  s1[is.na(t(s1))]<-NA
  b1<-mat$b
  b1[is.na(s1)]<-NA
 # imagenan(b1,zlim=c(-4,30),main=" Equitableb START: Intercept",xlab="Row", ylab="Col")

  rtest<-testEb(b1,s1)       #test corrected matrix for equitability
  cat("Equitableb: test values of least squares fits ",rtest$xm,rtest$xsd,"\n")

  #first run calc sd for each intercept as well
  Eb<-outer(1:nc,1:nc,Vectorize(Eintercept),b=as.data.frame(c(b1)),
            s=as.data.frame(c(s1)),nc=nc,p=as.data.frame(c(mat$pslope)))  #input matrices must be as data.frames output must be same length
  colnames(Eb)<-colnames(mat$b) ; rownames(Eb)<-rownames(mat$b)
  Eb1<-Eb
  Eb1sd<-outer(1:nc,1:nc,Vectorize(Esdintercept),b=as.data.frame(c(b1)),
               s=as.data.frame(c(s1)),nc=nc,p=as.data.frame(c(mat$pslope)))  # first iteration find errors for each slope
  colnames(Eb1sd)<-colnames(mat$s) ; rownames(Eb1sd)<-rownames(mat$s)
  Eb1N<-outer(1:nc,1:nc,Vectorize(bN),b=as.data.frame(c(b1)),
               s=as.data.frame(c(s1)),nc=nc,p=as.data.frame(c(mat$pslope)))  # first iteration find errors for each slope
  colnames(Eb1N)<-colnames(mat$s) ; rownames(Eb1N)<-rownames(mat$s)
  numrun=1
  rtest<-testEb(Eb,s1)
  #  maxrun=8 maxzero_std<-0.015, zero=0.001, maxrun=8
  bElist<-list(    #list of relevant intercept parameters
    b=Eb,
    numrun=numrun,
    rtestbm=rtest$xm ,     #xm and xsd
    rtestbsd=rtest$xsd,
    b1=Eb1,     #first iteration matrix
    bsd1=Eb1sd,     #first iteration matrix sd dev error for each intercept
    bN=Eb1N
  )

  bElist<-equitb(bElist,Eb,s1,mat$pslope)  # bElist is list and Eb is latest intercept matrix

  #after finishing put poor intercepts back in
  bElist$b[which(is.na(bElist$b))]<-mat$b[which(is.na(bElist$b))]
  bElist$b1[which(is.na(bElist$b1))]<-mat$b[which(is.na(bElist$b1))]
  bElist$bsd1[which(is.na(bElist$bsd1))]<-0
 # imagenan(bElist$b,zlim=c(-4,30),main=" Equitableb END: Intercept",xlab="Row", ylab="Col")
 # imagenan(bElist$b,zlim=c(0,4),main=" Equitableb: Final Intercept",xlab="Row", ylab="Col")
  return(bElist)
}# calculates equitable intercept matrix from information in mat. returns a list of matrices related to the equitable matrix constrcut

#' mean and std dev on NA data set
#'
#' Finds mean and standard deviaiton of 2 dimensional data set that contains NA values
#'
#' @param d  data set
#'
#' @return list of mean $m and standard deviation $std
#'
#' @examples
#' nastat(d=data.frame(cbind(c(1:4),c(2,5,NA,NA) )))  #
#' nastat(eg4())   # stats and image of example 4
#' nastat(eg7())   # stats and image of example 7
#'
#'
#' @export
nastat<-function(d){
  d<-as.matrix(d)
  m<-mean(c(d),na.rm = TRUE )
  std<-sd(c(d),na.rm = TRUE )
  cat("mean: ",m)
  cat(" std. dev.: ",std,"\n" )   #
  ms<-list(m=m,std=std)
  return(ms)
} #finds stats (mean and std dev) of  d  that has NA values

minmax<-function(d){
  mi<-min(c(d),na.rm = TRUE )
  ma<-max(c(d),na.rm = TRUE )
  cat("min: ",mi)
  cat(" max: ",ma,"\n" )   #
  ms<-list(mi=mi,ma=ma)
  return(ms)
  return(list(mi,ma))
} #finds stats (mean and std dev) of  d  that has NA values

runstats<-function(Tx,fracdim=FALSE){
  if(fracdim){
  cat("\nOriginal")
  frac_dim(orig=Tx$smat,main="Original Data")
  cat("Equitable Transform")
  frac_dim(orig=Tx$ET.x,main="Equitable Transform")

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
  }
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
} # finds stats of transforms and stats relative to the original data set

#' mean and std dev of transform differenced with the signal data set
#'
#' Finds mean and standard deviations of transforms and residuals between transfgorms and signal
#'
#' @param T_noise noisy transform information from transformE
#' @param Tx  signal transform information from transformE
#' @param extraf ignore
#'
#' @return None
#'
#' @examples
#' #first construct transfor of data and trnasform of signal
#' d<-eg4(2,2)
#' Td<-transformE(d)
#' d_noise<-d+rnorm(prod(dim(d)),mean=0,sd=(1/4*sd(d,na.rm=TRUE)))
#' Td_noise<-transformE(d_noise)
#' runstatsNS(Tx=Td_noise,T=Td)  #
#'
#' @export
runstatsNS<-function(Tx,T_noise,extraf=FALSE){
  cat("\nSIGNAL \noriginal signal\n")
  storig<-nastat(Tx$smat)
  sigorig<-as.numeric(storig[2])
  cat("Least squared transform \n")
  nastat(T_noise$l.s.x)

  cat("Equitable transform \n")
  stT<-nastat(T_noise$ET.x)
  sigT<-as.numeric(stT[2])

  cat("(Signal+noise) -Original signal  =Noise only\n")
  statN<-nastat(T_noise$smat-Tx$smat)
  sig0<-as.numeric(statN[2])

  cat("Least squared transform -Original signal\n")
  statls<-nastat(T_noise$l.s.x-Tx$smat)
  sigls<-as.numeric(statls[2])

  cat("Equitable transform -Original signal\n")
  nastat(T_noise$ET.x-Tx$smat)

  cat("Equitable transform using ref column (Ave usually)-Original signal\n")
  nastat(T_noise$Ave.ET.x-Tx$smat)

  a2S<-mean(Tx$l.s.s^2,na.rm=TRUE)   #average of square of slope
  a2<-mean(Tx$E.s^2,na.rm=TRUE)   #average of square of slope

  if(extraf){
  factora2<- sqrt(1/nrow(Tx$smat)+a2/ncol(Tx$smat))
  factor<- sqrt(1/(nrow(Tx$smat)-1)+1/(ncol(Tx$smat)-1))
  factor_row<- sqrt(1/nrow(Tx$smat))
  factor_col<- sqrt(1/ncol(Tx$smat))
  factora2S<-sqrt(1/nrow(Tx$smat)+a2S/ncol(Tx$smat))

  acolm<-colMeans(Tx$ET.Es,na.rm=TRUE)
  am<-mean(acolm^2, na.rm=TRUE)
  amfactor<-sqrt(1/nrow(Tx$smat)+am/ncol(Tx$smat))
  acolmabs<-colMeans(1-abs(Tx$E.s),na.rm=TRUE)
  amean<-mean(abs(acolmabs), na.rm=TRUE)

  sumNA<-sum(length(which(is.na(T_noise$smat))))

  Nt<-prod(dim(T_noise$smat))
  fr<-sumNA/Nt
  cat("\ntotal of NA transformed data is ",sumNA," with total ",Nt," Fraction of data set that is NA is ",fr )
  fac<-sqrt(1-fr)
  newM<-nrow(T_noise$smat)*fac
  newN<-ncol(T_noise$smat)*fac
  sdfactornew<- sqrt(1/(newN-1)+1/(newM-1))
  sdfactor<- sqrt(1/(nrow(T_noise$smat)-1)+1/(ncol(T_noise$smat)-1))
  cat("\n  col= ",ncol(T_noise$smat),"  row= ",nrow(T_noise$smat), " initial scale factor= ",sdfactor)
  cat("\neffective  col= ",newN,"effective  row= ",newM, " scale factor= ",sdfactornew, "final = ",sig0*sdfactornew)

 cat("\nfactor of noise reduction no slope : ",  factor, " final reduced noise could be ",sig0*factor)
 cat("\nav am ",am," factor  : ",  amfactor, " final reduced noise should be ",sig0*amfactor)
 cat("\n mean of 1-abs(slope)",amean )
 lm.ls_vs_sig <- lm(c(T_noise$l.s.x) ~ c(Tx$smat), na.action=na.exclude)
 b<-coef(lm.ls_vs_sig)[1]; a<-coef(lm.ls_vs_sig)[2]
 cat("\nLeast squared vs signal linear fit \nslope= ",a," intercept = ",b,"\n")
 #print(summary(lm.ls_vs_sig))
 #a<-(0.887)^2  ; b<- 0.54 ;storig<-2.42; sig0<-0.67;
 sdcalc<-sqrt((a-1)^2*sigorig^2+(a^2)*(sig0*factor)^2+b^2)
 ms<-mean(Tx$smat, na.rm=TRUE)
 sdcalc<-sqrt((a-1)^2*sigorig^2+(a^2)*(sig0*factor)^2+b^2  +(a-1)*b*ms)
 sdcalcE<-sqrt((1-1)^2*sigorig^2+(1^2)*(sig0*factor)^2+0^2)
 cat("\n fit l.s vs signal: 1-slope=",1-a, " intercept=",b," signal std dev ",sigorig,"noise sig reduced= ",sig0*factor,"l.s. Final calc=",sdcalc)
 cat("\n")
 #cat("\n fit:Equitable assume perfect fit: signal std dev ",sigorig,"noise sig reduced= ",sig0*factor," Final calc=",sdcalcE)
  }
}  # finds stats of transforms and stats relative to the signal data set

stats_residualsold<-function(Td_noise,
                          Td=NULL,Td_old=NULL,genname="Equitable",ylim=NULL,pf=TRUE,ipf=TRUE){
  if(is.null(Td_old))Td_old<-Td_noise
  nc<-ncol(Td_noise$smat);nr<-nrow(Td_noise$smat)
  xresiduals<-Td_old$smat-Td_noise$ET.x
  sdI_T<-sd(xresiduals,na.rm=TRUE)
  if(is.null(ylim)){
    ylim<-c(min(xresiduals,na.rm=TRUE),max(xresiduals,na.rm=TRUE))
  }
  dam1<-c(NA,length=nrow(Td_noise$ET.Es))
  for (r in 1:nrow(Td_noise$ET.Es))dam1[r]<-weighted.mean(Td_noise$ET.Es[r,],(1-Td_noise$ET.Ep[r,]),na.rm = TRUE)-1
  # #for (r in 1:nrow(Td_noise$ET.Es))dam1[r]<-weighted.mean(Td_noise$E.s[r,],(1-Td_noise$E.pslope[r,]),na.rm = TRUE)-1
  x<-mean(dam1^2,na.rm = TRUE)
  #F<-sqrt((1)/ncol(noise)+1/nrow(noise))
  Fp<-sqrt((1+x)/nc+1/nr)
  F<-sqrt((1)/nc+1/nr)
  sdnoise_simple<-sdI_T/sqrt(1-F^2)
  sdnoise_approx<-sdI_T/sqrt(1-Fp^2)
  cat("\n Simple noise std dev from Transform is ",sdnoise_simple )
  cat("\n Approximate noise std dev from Transform is ",sdnoise_approx )

  if(pf){
    plot(Td_noise$l.s.x, Td_old$smat-Td_noise$l.s.x,
         main=paste0("Comparing  Residuals vs ",genname," L.S. Transform"),
         xlab = "Transform Values", ylab = paste0("Residuals(Data - ",genname," L.S. Transform)"),ylim=ylim)
    abline(h=0, lty=2,lwd=2)
    lines(smooth.spline(Td_noise$l.s.x, Td_old$smat-Td_noise$l.s.x),lwd=4,lty=1)

    plot(Td_noise$ET.x, xresiduals,
         main=paste0("Comparing  Residuals vs ",genname," Transform"),
         xlab = "Transform Values", ylab = paste0("Residuals (Data - ",genname," Transform)"),ylim=ylim)
    abline(h=0, lty=2,lwd=2)
    lines(smooth.spline(Td_noise$ET.x, xresiduals),lwd=4,lty=1)
  }
  if(!is.null(Td)){  #signal defined
    noise<-Td_old$smat-Td$smat
    sd_noise<-sd(noise,na.rm=TRUE)
    cat("\n correct noise std dev is", sd_noise)
    cm<-colMeans(noise,na.rm=TRUE)
    rm<-rowMeans(noise,na.rm=TRUE)
    N_Ave<-matrix(NA,nrow=nrow(noise),ncol=ncol(noise))
    for (r in 1:nrow(N_Ave))for(c in 1:ncol(N_Ave))N_Ave[r,c]<-rm[r]+cm[c]
    N_Avet<-matrix(NA,nrow=nrow(noise),ncol=ncol(noise))
    for (r in 1:nrow(N_Ave))for(c in 1:ncol(N_Ave))N_Avet[r,c]<-rm[r]*dam1[c]   #uses slopes from transform of noisy data
    sqrt(sd(N_Avet,na.rm=TRUE)^2+sd(N_Ave,na.rm=TRUE)^2)
    N_Ave_plus<-N_Ave+N_Avet
    sd(N_Ave_plus,na.rm=TRUE)
    xSresiduals<-Td$smat-Td_noise$ET.x
    if(pf){
      plot(Td_noise$l.s.x, Td$smat-Td_noise$l.s.x,
           main=paste0("Comparing  Residuals from signal vs ",genname," L.S. Transform"),
           xlab = "Transform Values", ylab = paste0("Residuals (Signal - ",genname," L.S. Transform)"),ylim=ylim)
      abline(h=0, lty=2,lwd=2)
      lines(smooth.spline(Td_noise$l.s.x, Td$smat-Td_noise$l.s.x),lwd=4,lty=1)

      plot(Td_noise$ET.x, xSresiduals,
           main=paste0("Comparing  Residuals from signal vs ",genname," Transform"),
           xlab = "Transform Values", ylab = paste0("Residuals (Signal - ",genname," Transform)"),ylim=ylim)
      abline(h=0, lty=2,lwd=2)
      lines(smooth.spline(Td_noise$ET.x, xSresiduals),lwd=4,lty=1)
    }
    sdN_Ave<-sd(N_Ave,na.rm=TRUE)
    N_AveAprox<- (-1)*xSresiduals
    theory_sdN_Ave<-F*sd_noise
    theory_sdN_Ave_plus<-Fp*sd_noise
    sdN_AveAprox<-sd(N_AveAprox,na.rm=TRUE)    #T-S ~ N_Ave
    sdN_Ave_plus<-sd(N_Ave_plus,na.rm=TRUE)    #
    cat("\nstd of 2D noise averages is ",sdN_Ave)
    cat("\nstd of 2D noise averages+extra from slopes is ",sdN_Ave_plus)
    cat("\nTHEORY std of 2D noise averages is ",theory_sdN_Ave)
    cat("\nTHEORY std of 2D noise averages+extra from slopes is ",theory_sdN_Ave_plus)
    cat("\n Approx of std of 2D noise averages frorm trasform is ",sdN_AveAprox )
    if(ipf){
      imagenan(N_Ave,zlim=c(min(N_Ave,na.rm=TRUE),max(N_Ave,na.rm=TRUE) ),main="N_Ave= N(x)_tave + N_xave(t)")
      imagenan(N_Ave_plus,zlim=c(min(N_Ave,na.rm=TRUE),max(N_Ave,na.rm=TRUE) ),main="N_Ave_plus= N(x)_tave +dam1[y]xave* N_xave(t)+CS")
      imagenan(N_AveAprox,zlim=c(min(N_Ave,na.rm=TRUE),max(N_Ave,na.rm=TRUE) ),main=" Transform-Signal ~ N(x)_tave +(1+dam1[y]xave)* N_xave(t)")
    }
    if(pf){
      # plot(N_AveAprox, N_Ave-N_AveAprox,
      #      main=paste0("Comparing  Residuals of Noise averages vs ",genname," T-S"),
      #      xlab = "Transform Values", ylab = paste0("Residuals (Row Col Noise Ave - ",genname," T-S)"),ylim=ylim)
      # abline(h=0, lty=2,lwd=2)
      # lines(smooth.spline(N_AveAprox, N_Ave-N_AveAprox),lwd=4,lty=1)

      plot(N_AveAprox, N_Ave_plus-N_AveAprox,
           main=paste0("Comparing  Residuals of Noise averages(plus) vs ",genname," T-S"),
           xlab = "Transform Values", ylab = paste0("Residuals (Row Col Noise Ave(plus) - ",genname," T-S)"),ylim=ylim)
      abline(h=0, lty=2,lwd=2)
      lines(smooth.spline(N_AveAprox, N_Ave_plus-N_AveAprox),lwd=4,lty=1)
    }
  }

  stdvalues<-list(
    sd_noise=sd_noise, sdnoise_simple=sdnoise_simple ,sdnoise_approx=sdnoise_approx ,sdN_AveAprox=sdN_AveAprox,
    sdN_Ave=sdN_Ave, sdN_Ave_plus=sdN_Ave_plus,
    theory_sdN_Ave=theory_sdN_Ave, theory_sdN_Ave_plus=theory_sdN_Ave_plus)
  return(stdvalues)
}# functi

stats_residuals<-function(Td_noise,
                          Td=NULL,Td_old=NULL,genname="Equitable",ylim=NULL,pf=TRUE,ipf=TRUE,C=1.028){
 # cat("\n\n C= ", C)
  if(is.null(Td_old)){
    Td_old<-Td_noise
    } else{  # Td_noise has slopes that are not masked so mask them like in Td_old
      imagenan(Td_noise$ET.Es,zlim=c(-4,4),main="premask")
   Td_noise$ET.Es[is.na(Td_old$ET.Es)]<-NA
    imagenan(Td_noise$ET.Es,zlim=c(-4,4),main="postmask")
  }
  nc<-ncol(Td_noise$smat);nr<-nrow(Td_noise$smat)
  xresiduals<-Td_old$smat-Td_noise$ET.x
  sdI_T<-sd(xresiduals,na.rm=TRUE)
  if(is.null(ylim)){
    ylim<-c(min(xresiduals,na.rm=TRUE),max(xresiduals,na.rm=TRUE))
  }
  dam1<-c(NA,length=nrow(Td_noise$ET.Es))
  for (r in 1:nrow(Td_noise$ET.Es))dam1[r]<-weighted.mean((Td_noise$ET.Es[r,]),(1-Td_noise$ET.Ep[r,]),na.rm = TRUE)
  #for (r in 1:nrow(Td_noise$ET.Es))dam1[r]<-mean(abs(Td_noise$ET.Es[r,]),na.rm = TRUE)
  x<-mean((1-dam1)^2,na.rm = TRUE)
  #x<-mean((dam1)^2,na.rm = TRUE)
  #cat("\n mean of slope squared term is ", x)

  #F<-sqrt((C+x)/ncol(noise)+1/nrow(noise))
  #Fp<-sqrt((x)/nc+1/nr)
  Fp<-sqrt((C+x)/nc+1/nr)
  F<-sqrt((1)/nc+1/nr)
  sdnoise_simple<-sdI_T/sqrt(1-F^2)
  sdnoise_approx<-sdI_T/sqrt(1-Fp^2)
  #cat("\n Simple noise std dev from Transform is ",sdnoise_simple )
  #cat("\n Approximate noise std dev from Transform is ",sdnoise_approx )


  plot(Td_noise$l.s.x, Td_old$smat-Td_noise$l.s.x,
       main=paste0("Comparing  Residuals vs ",genname," L.S. Transform"),
       xlab = "Transform Values", ylab = paste0("Residuals(Data - ",genname," L.S. Transform)"),ylim=ylim)
  abline(h=0, lty=2,lwd=2)
  if(pf)lines(smooth.spline(Td_noise$l.s.x, Td_old$smat-Td_noise$l.s.x),lwd=4,lty=1)

  plot(Td_noise$ET.x, xresiduals,
       main=paste0("Comparing  Residuals vs ",genname," Transform"),
       xlab = "Transform Values", ylab = paste0("Residuals (Data - ",genname," Transform)"),ylim=ylim)
  abline(h=0, lty=2,lwd=2)
  if(pf)lines(smooth.spline(Td_noise$ET.x, xresiduals),lwd=4,lty=1)

  if(!is.null(Td)){  #signal defined
    noise<-Td_old$smat-Td$smat
    sd_noise<-sd(noise,na.rm=TRUE)
    cat("\n correct noise std dev is", sd_noise)
    cm<-colMeans(noise,na.rm=TRUE)
    rm<-rowMeans(noise,na.rm=TRUE)
    Srm<-rowMeans(Td$smat,na.rm=TRUE)
    N_Ave<-matrix(NA,nrow=nrow(noise),ncol=ncol(noise))
    for (r in 1:nrow(N_Ave))for(c in 1:ncol(N_Ave))N_Ave[r,c]<-rm[r]+cm[c]
    N_Avet<-matrix(NA,nrow=nrow(noise),ncol=ncol(noise))
    N_Ave_plus<-matrix(NA,nrow=nrow(noise),ncol=ncol(noise))
    #for (r in 1:nrow(N_Ave))for(c in 1:ncol(N_Ave))N_Avet[r,c]<-rm[r]*((-1+dam1[c]*sqrt(C)))   #uses slopes from transform of noisy data
     for (r in 1:nrow(N_Ave))for(c in 1:ncol(N_Ave))N_Ave_plus[r,c]<-cm[c]+rm[r]*(C+dam1[c])+(C-1)*(Srm[r]-mean(Td$smat,na.rm=TRUE))
    #sqrt(sd(N_Avet,na.rm=TRUE)^2+sd(N_Ave,na.rm=TRUE)^2)
   # N_Ave_plus<-N_Ave+N_Avet
    sd(N_Ave_plus,na.rm=TRUE)
    xSresiduals<-Td$smat-Td_noise$ET.x

    plot(Td_noise$l.s.x, Td$smat-Td_noise$l.s.x,
         main=paste0("Comparing  Residuals from signal vs ",genname," L.S. Transform"),
         xlab = "Transform Values", ylab = paste0("Residuals (Signal - ",genname," L.S. Transform)"),ylim=ylim)
    abline(h=0, lty=2,lwd=2)
    if(pf)lines(smooth.spline(Td_noise$l.s.x, Td$smat-Td_noise$l.s.x),lwd=4,lty=1)

    plot(Td_noise$ET.x, xSresiduals,
         main=paste0("Comparing  Residuals from signal vs ",genname," Transform"),
         xlab = "Transform Values", ylab = paste0("Residuals (Signal - ",genname," Transform)"),ylim=ylim)
    abline(h=0, lty=2,lwd=2)
    if(pf) lines(smooth.spline(Td_noise$ET.x, xSresiduals),lwd=4,lty=1)

    sdN_Ave<-sd(N_Ave,na.rm=TRUE)
    N_AveAprox<- (-1)*xSresiduals
    theory_sdN_Ave<-F*sd_noise
    theory_sdN_Ave_plus<-Fp*sd_noise+(C-1)*sd(Srm,na.rm=TRUE)
    sdN_AveAprox<-sd(N_AveAprox,na.rm=TRUE)    #T-S ~ N_Ave
    sdN_Ave_plus<-sd(N_Ave_plus,na.rm=TRUE)    #
    cat("\nstd of 2D noise averages is ",sdN_Ave)
    cat("\nstd of 2D noise averages+extra from slopes is ",sdN_Ave_plus)
    cat("\nTHEORY std of 2D noise averages is ",theory_sdN_Ave)
    cat("\nTRUE std of 2D noise averages is ",sdN_Ave)
    cat("\nTHEORY std of 2D noise averages+extra from slopes is ",theory_sdN_Ave_plus)
    cat("\n Approx of std of 2D noise averages from transform is ",sdN_AveAprox )
    if(ipf){
    imagenan(N_Ave,zlim=c(min(N_Ave,na.rm=TRUE),max(N_Ave,na.rm=TRUE) ),main="N_Ave= N(x)_tave + N_xave(t)")
    imagenan(N_Ave_plus,zlim=c(min(N_Ave,na.rm=TRUE),max(N_Ave,na.rm=TRUE) ),main="N_Ave_plus= cm[c]+rm[r]*(dam1[c]) +(C-1)*(Srm[r])")
    imagenan(N_AveAprox,zlim=c(min(N_Ave,na.rm=TRUE),max(N_Ave,na.rm=TRUE) ),main=" Transform-Signal ~ N(x)_tave +(dam1[y]xave)* N_xave(t)")


    # plot(N_AveAprox, N_Ave-N_AveAprox,
    #      main=paste0("Comparing  Residuals of Noise averages vs ",genname," T-S"),
    #      xlab = "Transform Values", ylab = paste0("Residuals (Row Col Noise Ave - ",genname," T-S)"),ylim=ylim)
    # abline(h=0, lty=2,lwd=2)
    # lines(smooth.spline(N_AveAprox, N_Ave-N_AveAprox),lwd=4,lty=1)

        ylim<-c(min(N_Ave-N_AveAprox,na.rm=TRUE),max(N_Ave-N_AveAprox,na.rm=TRUE))

    plot(N_AveAprox, N_Ave_plus-N_AveAprox,
         main=paste0("Comparing  Residuals of Noise averages(plus) vs ",genname," T-S"),
         xlab = "Transform Values", ylab = paste0("Residuals (Row Col Noise Ave(plus) - ",genname," T-S)"),ylim=ylim)
    abline(h=0, lty=2,lwd=2)
    if(pf) lines(smooth.spline(N_AveAprox, N_Ave_plus-N_AveAprox),lwd=4,lty=1)
    plot(N_AveAprox, N_Ave-N_AveAprox,
         main=paste0("Comparing  Residuals of Noise averages vs ",genname," T-S"),
         xlab = "Transform Values", ylab = paste0("Residuals (Row Col Noise Ave - ",genname," T-S)"),ylim=ylim)
    abline(h=0, lty=2,lwd=2)
    if(pf)lines(smooth.spline(N_AveAprox, N_Ave-N_AveAprox),lwd=4,lty=1)
    }
    stdvalues<-list(
      sd_noise=sd_noise, sdnoise_simple=sdnoise_simple ,sdnoise_approx=sdnoise_approx ,sdN_AveAprox=sdN_AveAprox,
      sdN_Ave=sdN_Ave, sdN_Ave_plus=sdN_Ave_plus,
      theory_sdN_Ave=theory_sdN_Ave, theory_sdN_Ave_plus=theory_sdN_Ave_plus)
  } else {
    stdvalues<-list(
     sdnoise_simple=sdnoise_simple ,sdnoise_approx=sdnoise_approx )
  }


  return(stdvalues)
}# function returning different sd values and plotting Residual relationships

parallel_test<-function(x,y,slope=1,scritical=0.1,pcritical=1e-5){

  fit <- lm(y ~ x, na.action=na.exclude )
  # Compute Summary with statistics
  sfit<- summary(fit)
  # Compute t- H0: intercept=slope. The estimation of coefficients and their s.d. are in sfit$coefficients
  if(nrow(sfit$coefficients)==2) {
  tstats <- (slope-sfit$coefficients[2,1])/sfit$coefficients[2,2]
  # Calculates two tailed probability
  pval<- 2 * pt(abs(tstats), df = df.residual(fit), lower.tail = FALSE)
  if(sfit$coefficients[2,"Pr(>|t|)"]<pcritical ){
    if( abs((slope-sfit$coefficients[2,1])/slope)<scritical)pval=1 else pval=0
  }
  } else pval<-NA
  if(is.nan(pval))pval<-NA
  #print(pval)
  #cat("\n p value for the null hypothesis that the slope is a slope of 1 ",pval)
  return(pval)
}# pvalues less than 0.05 indicate NON-parallel slopes  Null hypothesis is a slope of default=1
#note that l.s. fits give inaccurate slopes versus equitbale ones
#if perfect fit then "parallel"(same slope) if slope within 1% of test slope(1) else different

parallel_Etest<-function(Es,p,p_par,slope=1,scritical=0.1,pcritical=1e-5,pflag=FALSE){
  if(pflag)imagenan(p_par,zlim=c(0.01,0.05),outside.above.color='red',main="Enter Etest :Parallel=red")
  #imagenan(p,main="Prob for no correlation")
  for (r in 1:nrow(Es)){
    for (c in 1:ncol(Es)){
      pv<-p[r,c]
      if(!is.na(pv)&& !is.nan(pv) && !is.infinite(pv)){
    if(pv<pcritical && !is.na(Es[r,c])  && !is.nan(Es[r,c])){

      if( abs((slope-Es[r,c])/slope)<scritical)p_par[r,c]=1 else p_par[r,c]=0
    }
      }
    }
  }
  if(pflag){
    imagenan(p_par,zlim=c(0.01,0.05),outside.above.color='red',main="Exit Etest :Parallel=red")
  cat("\n(At 95% Confidence: Proportion of parallel slopes(NULL)",(length(which(p_par>0.05)) )/length(p_par[which(!is.na(p_par))]) ) #parallel            .95
  cat(" Non parallel ",(length(which(p_par<=0.05)) )/length(p_par[which(!is.na(p_par))]) ,"\n") #not parallel 95%
  cat("    98%: Proportion of parallel slopes ",(length(which(p_par>0.02)) )/length(p_par[which(!is.na(p_par))]) ) #parallel            .95
  cat(" Non parallel ",(length(which(p_par<=0.02)) )/length(p_par[which(!is.na(p_par))]) ,"\n\n") #not parallel 95%
  }
   return(p_par)
  }# for all slopes with excellent fits check if Equitable slope is within bounds  (since l.s slopes often arent even for good fits)
#if perfect fit then "par

newxfac<-function(dissimilarity=Td6_9$ET.x,sname,dendfactor=NULL){

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
} # creates xfac consistent with transform data (Row_Aves etc)

addave<-function(d,cAve=FALSE){     #add  averages over columns (applied for each row) d[r,] and apend onto dataset
  newd<-d
  newd<-cbind(d,rowMeans(d, na.rm=TRUE))
  newd[is.nan(newd)]<-NA
  if(cAve){
    newd<-rbind(newd,colMeans(newd, na.rm=TRUE))
    rownames(newd)[nrow(newd)]<-paste0("Col_Ave")
  }
 colnames(newd)[ncol(newd)]<-paste0("Row_Ave")
        return(newd)
}   # add  row and column averages to the data sets so the equitable matrices are linked to averages


#' Equitable transform
#'
#' Creates an equitable transform and returns information regarding it
#'
#' @param d 2D data to be transformed
#' @param Ave Include a new column with Row averages \code{"TRUE"}(Default) or \code{"FALSE"}
#' @param cAve Include a new row with Column averages \code{"TRUE"} or \code{"FALSE"}(Default)
#' @param Zero if TRUE subtract a zero from data set. Default FALSE
#' @param zero Value to be subtracted off of all data when transforming Default=0
#' @param diagonal Include diagonals of matrices when transforming \code{"TRUE"}(Default) or \code{"FALSE"}
#' @param imageplot TRUE plots image of data Default FALSE
#' @param old Default NULL (could use old transform slopes to mask new slope transform)
#'
#' @return Output from the TransformE function
#' @return Running Td<-transformE(d) gives Td that contains several variables  (see summary(Td))
#' @return (access variables via Td$variable_name ).
#' @return Td$smat is original data set         :view using imagenan(Td$smat)
#' @return Equitable Transform: Td$ET.x  matrix of transformed data      :view using imagenan(Td$ET.x)
#' @return least squared Transform: Td$l.s.x                              :view using imagenan(Td$l.s.x)
#' @return Equitable Transform based only on average column: Td$Ave.ET.x (assumes Ave=TRUE)
#' @return l.s. prefix indicates LEAST SQUARES result
#' @return      s=slope, b=intercept                       sse =std error of slope       bse=std error of intercept
#' @return      r2=coef. of determination                  N: # of points in fit         pslope: prob. for no correlation
#' @return      node== indication if fit is due to a node  p_par:sequences approximately  parallel
#' @return      zero: subtracted value
#' @return  Examples of LEAST SQUARES variables:
#' @return  l.s.s      l.s.sse
#' @return  l.s.b      l.s.bse   l.s.r2    l.s.N      l.s.pslope l.s.node l.s.p_par l.s.zero
#' @return # e.g. view using  imagenan(Td$l.s.s)
#' @return  l.s.x= Least squared transform       std. dev. errors at each point: l.s.xsd
#' @return  l.s.Es l.s.Eb   l.s.Ep    : masked matrices of best values included slope=Es  intercept=Eb prob= Ep
#' @return  E prefix indicates EQUITABLE result
#' @return  Equitable slopes: E.s                 intercepts: E.b
#' @return            errors: E.rtestxsd,                     E.rtestbsd
#' @return #view using  imagenan(Td$E.s)
#' @return     with convergence information (E.rtestxm,E.rtestbm) Values of r^2 after convergence functions
#' @return     E.numrun: # runs to get convergence
#' @return     first iteration slopes/intercepts: (E.s1,E.b1) with std. dev. errors  (E.sd1, E.bsd1)
#' @return  E.s        E.numrun    E.rtestxm   E.rtestxsd  E.s1       E.sd1  E.sN   E.snode
#' @return  E.b        E.numrun    E.rtestbm   E.rtestbsd  E.b1       E.bsd1  E.bN
#' @return  ET.x: Equitable tranform () with std. dev. errors at each point: ET.xsd
#' @return  ET.N: number of points averaged to get point
#' @return  ET.x       ET.xsd    ET.N
#' @return  ET.Es ET.Eb   ET.Ep             : masked matrices included slope=Es  interceprt=Eb prob= Ep
#' @return  Ave.ET.x   Ave.ET.xsd Ave.ET.N Ave.ET.Es Ave.ET.Eb   Ave.ET.Ep
#' @return  transform based on only average column: masked matrices included
#'
#' @examples
#' # Find a transform using a signal with no noise and then
#' # add noise and show the results for a noisy data set.
#' # A researcher with a data set can simply use transformE and plotsummary
#' #  on their data set placed in variable d
#' # d is an example (4) of a two dimensional separable signal
#' d<-eg4(3,3)
#' Td<-transformE(d, Ave=TRUE)  #Run and equitable transform on the data
#' #creates summary plots of the data comparing sequences in various ways
#' plotsummary(Td)
#'                #add noise to this signal data set
#' #find the std dev of the overall signal and add normally distributed noise
#' # that has a std. dev that is some fraction (fac) of this signal std dev
#' #let the fraction be 1/2 the standard deviaiton of the signal
#' #add to signal a normal distribution of noise with this std dev.
#' d_noise<-d+rnorm(prod(dim(d)),mean=0,sd=1/2*sd(d,na.rm=TRUE))
#'       # Once you have a data set (named d_noise) and
#'       #you want to find if there is an underlying pattern, run the transform
#' #Ave= TRUE includes an additional sequence of averages, if not desired set to FALSE
#' Td_noise<-transformE(d_noise, Ave=TRUE)
#'  #summary plots of the transform data compared to the thwe original
#' plotsummary(Td_noise)
#'  # if you want to include units for the rows columns and data add in the units
#' plotsummary(Td_noise,row_unit="Year", col_unit="Day Number",z_unit="Temperature")
#' #shows statistics relative to Signal :Cnonly run this if you already have a signal
#' runstatsNS(Td,Td_noise)
#' #shows signal along with noisy data and transforms
#' #plotsummary puts togetwher numerous calls to plotsome, and plotsquares
#' plotsummary(Td_noise,Td)
#' #shows signal along with noisy data and transforms
#' plotsummary(Td_noise,Td)
#' #plotsummary puts togetwher numerous calls to plotsome, and plotsquares
#'
#' plotsummary(Td_noise,Td,row_unit="Year", col_unit="Day Number",z_unit="Temperature")
#' plotsummary(Td_noise,Td,row_unit="Year", col_unit="Day Number",z_unit="Temperature")
#'
#' #if you already know the "signal" (in d), create the separable
#' # Transforms into Td
#'       #Any of the above stepts could be run with data sets
#'       # having less than about 450 sequences
#'       # other examples include eg0, eg1,eg2,eg3,e4,eg5, eg6,eg7,eg8,egrand
#'       #resolutions of these examples can be altered by changing rmult and cmult
#' d<-eg5(1,1)
#' d<-eg5(2,2)
#' d<-eg5(5,5)
#' d<-eg5(15,15)
#' d<-eg5(1,15)
#' d<-eg5(15,1)
#' @export
transformE<-function(d, Ave=TRUE,cAve=FALSE,Zero=FALSE,zero=NULL,diagonal=TRUE,imageplot=FALSE,old=NULL){
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
}  #creates all the equitable transforms and matrices associated with the data set d

reviseb<-function(zero,b_old,s) {
  cat("\n",ncol(b_old))
  cat("\n",ncol(s))
  cat("\n",zero)
  b<-b_old-zero*(1-s)
  return(b)
}
reviseb_witherror<-function(rownum=NULL,Td,ref=ncol(Td$ET.x)) {
  b_old<-Td$ET.Eb
  b_old1<-Td$E.b
  s<-Td$ET.Es
  s1<-Td$E.s
  oldzero<-Td$l.s.zero
  if(is.null(ref))ref<-ncol(Td$ET.x)
  if(!is.null(rownum)) zero<-Td$ET.x[rownum,ref]-oldzero else zero<-0 #unchanged if rownum not set
  # cat("\nref ",ref)
  # cat("\nold zero ", oldzero," orig data zero ", Td$ET.x[rownum,ref]," final zero set to ",zero)
  Td$ET.Eb<-b_old-zero*(1-s)
  Td$E.b<-b_old1-zero*(1-s1)
  ssd<-Td$E.sd1 #/sqrt(Td$E.sN)
    bsd<-Td$E.bsd1 #/sqrt(Td$E.bN)
    zsd<-Td$ET.xsd[rownum,ref]/sqrt(Td$ET.EN[rownum,ref])
  #Td$E.bsd1<-sqrt(((s-1)*ssd)^2 +((zero-oldzero)*zsd)^2+bsd^2)    #
   #Td$E.bsd1<-sqrt(((zero)*zsd)^2+bsd^2)
  #Td$E.bsd1<-sqrt( +bsd^2)



 Td$E.bsd1<-sqrt(((s-1)*ssd)^2 +bsd^2)    #errors due to slopes and intercepts included sqrt( (dF/ds*ssd)^2+(dF/db*bsd)^2 )
  Td$l.s.zero<-Td$ET.x[rownum,ref]
 # imagenan(Td$E.bsd1)
  return(Td)
}

rows_via_intercept<-function(Td_noise,rowlist=NULL,pnum=3,ref=NULL,z_unit=NULL,AVE=TRUE){

  if(is.null(rowlist))rowlist<-seq( nrow(Td_noise$smat),1, by=(-1*nrow(Td_noise$smat)/pnum))

  if(is.null(ref))ref<-ncol(Td_noise$smat)
  sdf<-1.4*(max(Td_noise$ET.Eb[,ref],na.rm=TRUE)-min(Td_noise$ET.Eb[,ref],na.rm=TRUE))
  for( rownum in rowlist){
    Tdnew<-reviseb_witherror(rownum=rownum,Td_noise,ref=ref)
    #sdf<-1.1*(max(Tdnew$ET.Eb[,ref],na.rm=TRUE)-min(Tdnew$ET.Eb[,ref],na.rm=TRUE))
    zero<-Tdnew$l.s.zero
    #num<-c((ref-1):ref)
    num<-c((rownum),rownum)
    if(AVE){
      limits=c(zero-sdf,zero+sdf)
      blimits=c(-sdf,+sdf)
      }else {
      limits=c(zero-sdf,zero)
      blimits=c(-sdf,0.3*sdf)
    }
    plotsome(Tdnew,images=FALSE,indiv=TRUE,num=num,transpose=TRUE,errb=TRUE,stderror=TRUE, of=TRUE,limits=limits,
             genname=paste0("Row value=",floor(rownum)," zero= ",round(zero,digits=1),"\n"),
             row_unit="", col_unit=paste0("Data Column)"),z_unit=z_unit) #images and c
    num<-c(ref,ref)
    plotsquares(Tdnew,num=num,images=FALSE,indiv=TRUE, of=FALSE,errb=TRUE,stderror=TRUE,slimits=c(0,1.2) ,blimits=blimits,
                main=paste0("Row value=",floor(rownum),"\nzero= ",round(zero,digits=1)),psf=FALSE,
                row_unit="", col_unit=paste0("Column (Row value= ",floor(rownum)," zero= ",round(zero,digits=1)),z_unit=z_unit)

     num<-c((rownum),rownum)
    plotsome(Tdnew,images=FALSE,indiv=TRUE,num=num,transpose=TRUE,errb=TRUE, of=TRUE,limits=limits,
             genname=paste0("Row value=",floor(rownum)," zero= ",round(zero,digits=1),"\n"),
             row_unit="", col_unit=paste0("Data Column)"),z_unit=z_unit) #images and c
    num<-c(ref,ref)
    plotsquares(Tdnew,num=num,images=FALSE,indiv=TRUE, of=FALSE,errb=TRUE,slimits=c(0,1.2) ,blimits=blimits,
                main=paste0("Row value=",floor(rownum),"\nzero= ",round(zero,digits=1)),psf=FALSE,
                row_unit="", col_unit=paste0("Column (Row value= ",floor(rownum)," zero= ",round(zero,digits=1)),z_unit=z_unit)

  }

}

transave1<-function(smat,TE,equita=TRUE,x=NULL,t=NULL,diagonal=TRUE,Zero=FALSE){
  Em<-list(TE$l.s.s ,TE$l.s.sse ,TE$l.s.b   ,
           TE$l.s.bse ,TE$l.s.r2 ,TE$l.s.N  ,TE$l.s.pslope ,TE$l.s.node, TE$E.s,TE$E.b )
  names(Em)<-c( "s","sse","b","bse","r2","N","pslope","node","p_par","E.s","E.b")
  if(Zero)zero<-TE$l.s.zero else zero=0
  cat("\ntransave1: zero used is ",zero)

  ETave<-transave(smat,Em,equita=equita,x=x,t=t,diagonal=diagonal,zero=zero)
  return(ETave)
}# transforms data set smat using equitable output from transformE (TE)

transave<-function(smat,Em,equita=TRUE,x=NULL,t=NULL,diagonal=TRUE,zero=0){

  d_ave<-xtdata(smat,x=x,t=t)

      d_ave<-d_ave-zero
      cat("\ntransave: zero used is ",zero)
  ETave<-transf(d_ave,Em,equita=equita,diagonal=diagonal)   #make Equitable transform  ET$x and std dev in ET$xsd
  names(ETave)<-paste0("Ave.ET.",names(ETave))
    ETave$Ave.ET.x<-ETave$Ave.ET.x+zero

  return(ETave)
}# transforms data set (smat) using  output used in transformE (Em)

multiTI<-function(Td_noise,numrun=1,maxrun=5,minme=0.001, minstd=0.015){

  if(numrun==1){imagenan(Td_noise$smat,main="Original"); imagenan(Td_noise$ET.x,main="T[1]=T(I)")}
  cat("\n\nmultiTI: run ",numrun)

  if(numrun>maxrun){
    cat("\n\n\nNo Convergence: ending at numrun=",numrun)
    if(!is.null(Td2)){
      Td2$Ave.ET.x<-ET2$Ave.ET.x
      Td2$Ave.ET.xsd<-ET2$Ave.ET.xsd
      Td2$Ave.ET.Es<-ET2$Ave.ET.Es
      Td2$Ave.ET.Eb<-ET2$Ave.ET.Eb
      Td2$Ave.ET.Ep <-ET2$Ave.ET.Ep
    } else Td2<-Td_noise
    return(Td_noise)
  }
  ET2<-transave1(Td_noise$ET.x,Td_noise,x=1:ncol(Td_noise$ET.x),equita=TRUE,diagonal=TRUE)
  #summary(ET1)
  me<-abs(mean((ET2$Ave.ET.x-Td_noise$ET.x)/Td_noise$ET.x,na.rm=TRUE))
  std<-sd((ET2$Ave.ET.x-Td_noise$ET.x)/Td_noise$ET.x,na.rm=TRUE)
  cat("\n\nmean of relative error",me)
  cat("\nsd of relative error",std)
  imagenan(ET2$Ave.ET.x,main=paste0("T(",numrun+1,")=T(T[",numrun,"])"))
  Td2<-Td_noise
  Td2$ET.x<-ET2$Ave.ET.x
  Td2$ET.xsd<-ET2$Ave.ET.xsd
  Td2$ET.Es<-ET2$Ave.ET.Es
  Td2$ET.Eb<-ET2$Ave.ET.Eb
  Td2$ET.Ep<-ET2$Ave.ET.Ep


  #stats s
  numrun<-numrun+1
  if(me< minme && std<minstd){
    cat("\n\n\nCONVERGENCE REACHED: T[n]=T[n-1] or T[n-1]=T(T[n-1])at numrun=",numrun)
    Td2$Ave.ET.x<-ET2$Ave.ET.x
    Td2$Ave.ET.xsd<-ET2$Ave.ET.xsd
    Td2$Ave.ET.Es<-ET2$Ave.ET.Es
    Td2$Ave.ET.Eb<-ET2$Ave.ET.Eb
    Td2$Ave.ET.Ep <-ET2$Ave.ET.Ep
    return(Td2)
  }

  multiTI(Td_noise=Td2,numrun=numrun)

} # transform using old slopes intercepts applied to the transformed data

multiT<-function(Td_noise,numrun=1,maxrun=5,minme=0.001, minstd=0.015){

  if(numrun==1){imagenan(Td_noise$smat,main="Original"); imagenan(Td_noise$ET.x,main="T[1]=T(I)")}
  cat("\n\nmultiTI: run ",numrun)

  if(numrun>maxrun){
    cat("\n\n\nNo Convergence: ending at numrun=",numrun)
    if(is.null(Td2)) Td2<-Td_noise
    return(Td2)
  }
  Td2<-transformE(Td_noise$ET.x, Ave=FALSE,diagonal=FALSE,old=Td_noise)

  me<-abs(mean((Td2$ET.x-Td_noise$ET.x)/Td_noise$ET.x,na.rm=TRUE))
  std<-sd((Td2$ET.x-Td_noise$ET.x)/Td_noise$ET.x,na.rm=TRUE)
  cat("\n\nmean of relative error",me)
  cat("\nsd of relative error",std)
  imagenan(Td2$ET.x,main=paste0("T(",numrun+1,")=T(T[",numrun,"])"))

  #stats s
  numrun<-numrun+1
  if(me< minme && std<minstd){
    cat("\n\n\nCONVERGENCE REACHED: T[n]=T[n-1] or T[n-1]=T(T[n-1])at numrun=",numrun)
    return(Td2)
  }

  multiT(Td_noise=Td2,numrun=numrun)

} # transform using old slopes intercepts applied to the transformed da

xtdata<-function(I,x=NULL,t=NULL,imageplot=FALSE) {
  if(is.null(x)) x<-ncol(I)    #colnames(s)[nrow(s)]
  if(is.null(t)) t<-1:nrow(I)
  if(x[1]=="max"){
    x<-floor(which(abs(I)==max(abs(I),na.rm=TRUE))/nrow(I))
    cat("\nxtdata: x set to max row is ",x)
  }
  # cat("\nxtdata: x set to ",x)
  # cat("\nxtdata: t set to ",t)
  if(!is.null(x) && !is.null(t)){
    d<-matrix(NA,nrow=nrow(I),ncol=ncol(I))
    d[t,x]<-I[t,x]
    rownames(d)<-rownames(I)
    colnames(d)<-colnames(I)
    zlimits<-c(min(d,na.rm=TRUE),max(I,na.rm=TRUE)+1)
    if(imageplot)imagenan(d,main=paste0("Original "), zlim=zlimits/1)
  }
  return(d)
} # creates data set from initial data I that is based on the row (t) and column (x) vectors

T<-function(rmult,cmult,
            FUN=eg4, noise=TRUE,diagonal=TRUE){
  d<-FUN(rmult,cmult)
  if(noise){
    d_noise<-jitter(c(d), factor = 5, amount = 0)
    #jitter same as runif :runif(n, min = A, max = B)  #uniform distribution between 0 and 1   std dev =sqrt((B-A)^2/12)
    # factor=5 amount=0  jitter is runif(n, -amount, amount)      amount =0  <- factor * z/50 (same as S).  z <- max(x) - min(x)
    # 1/10 (max-min)   std=(max-min) sqrt(1/10*delta^2/12)   delta*sqrt(1/120)=0.09  ~10%
    d_noise<-matrix(d_noise,nrow=nrow(d),ncol=ncol(d))
    rownames(d_noise)<-rownames(d); colnames(d_noise)<-colnames(d)
    d<-d_noise
    imagenan(d,main=paste0("Noise: rmult= ",rmult," cmult= ",cmult," Jitter=5"))
  }

  #Td<-transformE(d,minp=0.5,mins=0,minfinal=0, Ave=TRUE,diagonal=diagonal)   #best?     most tests Sat night Nov 26
  Td<-transformE(d,Ave=TRUE,diagonal=diagonal)
  runstats(Td)
  return(Td)
}#makes a variable resolution 2d data set based on f,g, and u in FUN with or without JITTER noise

Tnorm<-function(rmult,cmult,
                FUN=eg4,fac=0.5, noise=TRUE,
                diagonal=TRUE,Ave=TRUE,Zero=FALSE){
  d<-FUN(rmult,cmult)
  if(noise){
    sdd<-sd(d,na.rm=TRUE)
    d_noise<-d+rnorm(prod(dim(d)),mean=0,sd=(fac*sdd))    #normal distribution with std dev of fac*d's std dev
    #d_noise<-jitter(c(d), factor = 5, amount = 0)
    d_noise<-matrix(d_noise,nrow=nrow(d),ncol=ncol(d))
    rownames(d_noise)<-rownames(d); colnames(d_noise)<-colnames(d)
    cat(sd(d),sd(d_noise),sd(d-d_noise))
    d<-d_noise
    imagenan(d,main=paste0("Noise: rmult= ",rmult," cmult= ",cmult," fac=",fac))

  }

  Td<-transformE(d,diagonal=diagonal,Ave=Ave,Zero=Zero)   #best?     most tests Sat night Nov 26

  runstats(Td)
  return(Td)
} #makes a 2d data set with normally distributed noise (over whole data set)

TnormNA<-function(rmult,cmult,
                FUN=eg4,fac=0.5, noise=TRUE,NAfrac=NULL,
                diagonal=TRUE,Ave=TRUE,Zero=FALSE,imageplot=FALSE){
  d<-FUN(rmult,cmult)
  imagenan(d)
  if(noise){
    sdd<-sd(d,na.rm=TRUE)
    dn<-d+rnorm(prod(dim(d)),mean=0,sd=fac*sdd)    #normal distribution with std dev of fac*d's std dev

    dn<-matrix(dn,nrow=nrow(d),ncol=ncol(d))
    rownames(dn)<-rownames(d); colnames(dn)<-colnames(d)

    if(!is.null(NAfrac)){
    numspaces<-NAfrac*prod(dim(dn))
    cat("\nadding ",numspaces," NA values to ", prod(dim(dn))," total values")
    spaces<-sample(1:prod(dim(dn)), numspaces, replace=FALSE)

      #runif(numspaces, min = 1, max = prod(dim(dn)))  #uniform distribution between 0 and 1   std dev =sqrt((B-A)^2/12)
    dnNA<-dn
    dnNA[spaces]<-NA
    dNA<-d
    dNA[spaces]<-NA
    } else{
      dnNA<-NULL
      dNA<-NULL
    }
    if(imageplot){
    imagenan(d,main=paste0("Signal: rmult= ",rmult," cmult= ",cmult," fac=",fac))
      if(!is.null(NAfrac))imagenan(dNA,main=paste0("Signal with NAs : rmult= ",rmult," cmult= ",cmult,"\nfac=",fac, " fraction of NA=",NAfrac))
    imagenan(dn,main=paste0("Noise: rmult= ",rmult," cmult= ",cmult," fac=",fac))
    if(!is.null(NAfrac))imagenan(dnNA,main=paste0("Noise with NAs : rmult= ",rmult," cmult= ",cmult,"\nfac=",fac, " fraction of NA=",NAfrac))
    }
  }

  Td<-transformE(d,diagonal=diagonal,Ave=Ave,Zero=Zero)   #best?     most tests Sat night Nov 26
  cat("\n\nSTATISTICS OF SIGNAL\n")
  runstats(Td)
  if(!is.null(NAfrac)){
  TdNA<-transformE(dNA,diagonal=diagonal,Ave=Ave,Zero=Zero)
  cat("\n\nSTATISTICS OF SIGNAL WITH NAS\n")
  runstats(TdNA)
  # if(mean(TdNA$l.s.r2,na.rm=TRUE)!=1){
  # calc_pca(x=TdNA$smat,main=" on Original Data TdNA")
  # calc_pca(x=TdNA$ET.x,main=" on Equitable Transform TdNA")
  # }
  }
   Tdn<-transformE(dn,diagonal=diagonal,Ave=Ave,Zero=Zero)   #best?     most tests Sat night Nov 26
   cat("\n\nSTATISTICS OF SIGNAL WITH NOISE\n")
   runstats(Tdn)
   cat("\n\nSTATISTICS OF SIGNAL WITH NOISE COMPARED TO SIGNAL\n")
   runstatsNS(Td,Tdn)
   # if(mean(Tdn$l.s.r2,na.rm=TRUE)!=1){
   # calc_pca(x=Tdn$smat,main=" on Original Data Tdn")
   # calc_pca(x=Tdn$ET.x,main=" on Equitable Transform Tdn")
   # }
   if(!is.null(NAfrac)){
     TdnNA<-transformE(dnNA,diagonal=diagonal,Ave=Ave,Zero=Zero)
     cat("\n\nSTATISTICS OF SIGNAL WITH NOISE WITH NAS\n")
     runstats(TdnNA)

     cat("\n\nSTATISTICS OF SIGNAL WITH NOISE WITH NAS COMPARED TO SIGNAL WITH NAS\n")
     runstatsNS(TdNA,TdnNA)
     # if(mean(TdnNA$l.s.r2,na.rm=TRUE)!=1){
     # calc_pca(x=TdnNA$smat,main=" on Original Data TdnNA")
     # calc_pca(x=TdnNA$ET.x,main=" on Equitable Transform TdnNA")
     # }
     cat("\n\nSTATISTICS OF SIGNAL WITH NOISE WITH NAS COMPARED TO SIGNAL\n")
     runstatsNS(Td,TdnNA)
   } else{
     TdNA<-NULL
     TdnNA<-NULL
   }
 trans4<-list(Td=Td,Tdn=Tdn,TdNA=TdNA,TdnNA=TdnNA,rmult=rmult,cmult=cmult,fac=fac,NAfrac=NAfrac)
# plotsummary(Tdn)
 # plotsummary(TdnNA,Td)
 # imagenan(d,main=paste0("Signal: rmult= ",rmult," cmult= ",cmult," fac=",fac))
 # imagenan(dn,main=paste0("Noise: rmult= ",rmult," cmult= ",cmult," fac=",fac))
 # imagenan(dNA,main=paste0("Signal with NAs : rmult= ",rmult," cmult= ",cmult,"\nfac=",fac, " fraction of NA=",NAfrac))
 # imagenan(dnNA,main=paste0("Noise with NAs : rmult= ",rmult," cmult= ",cmult,"\nfac=",fac, " fraction of NA=",NAfrac))
  return(trans4)
} #makes a 2d data set with normally distributed noise (over whole data set)

Tnormcol<-function(rmult,cmult,
                   FUN=eg4,fac=0.5, noise=TRUE,
                   diagonal=TRUE){
  d<-FUN(rmult,cmult)
  if(noise){
    d_noise<-matrix(d,nrow=nrow(d),ncol=ncol(d))
    sdd<-sd(d,na.rm=TRUE)
    for(col in 1:ncol(d)){
    d_noise[,col]<-d[,col]+rnorm(nrow(d),mean=0,sd=fac*sdd)    #normal distribution with std dev of fac*d's std dev
    }
    d_noise<-matrix(d_noise,nrow=nrow(d),ncol=ncol(d))
    rownames(d_noise)<-rownames(d); colnames(d_noise)<-colnames(d)
    d<-d_noise
    imagenan(d,main=paste0("Noise: rmult= ",rmult," cmult= ",cmult," fac=",fac))
  }
  #Td_noise<-transformE(d_noise,minp=0.5,mins=0,minfinal=1/5, Ave=TRUE)   #best?
  Td<-transformE(d, Ave=TRUE,diagonal=diagonal)   #best?     most tests Sat night Nov 26
  # Td<-transformE(d,minp=0.5,mins= 0,minfinal=1/15, Ave=TRUE)   #best?   #used a great deal for development Nov 27
  # Td_noise<-transformE(d_noise,minp=0.7,mins=0,minfinal=1/5)
  runstats(Td)
  return(Td)
} #generate 2d data set with normally distributed noise:each column has mean=0 (average over time)

Tnormrow<-function(rmult,cmult,
                   FUN=eg4,fac=0.5, noise=TRUE,
                   diagonal=TRUE){
  d<-FUN(rmult,cmult)
  if(noise){
    d_noise<-matrix(d,nrow=nrow(d),ncol=ncol(d))
    sdd<-sd(d,na.rm=TRUE)
    for(row in 1:nrow(d)){
      d_noise[row,]<-d[row,]+rnorm(ncol(d),mean=0,sd=fac*sdd)    #normal distribution with std dev of fac*d's std dev
    }
    d_noise<-matrix(d_noise,nrow=nrow(d),ncol=ncol(d))
    rownames(d_noise)<-rownames(d); colnames(d_noise)<-colnames(d)
    d<-d_noise
    imagenan(d,main=paste0("Noise: rmult= ",rmult," cmult= ",cmult," fac=",fac))
  }
  #Td_noise<-transformE(d_noise,minp=0.5,mins=0,minfinal=1/5, Ave=TRUE)   #best?
  Td<-transformE(d, Ave=TRUE,diagonal=diagonal)   #best?     most tests Sat night Nov 26
  # Td<-transformE(d,minp=0.5,mins= 0,minfinal=1/15, Ave=TRUE)   #best?   #used a great deal for development Nov 27
  # Td_noise<-transformE(d_noise,minp=0.7,mins=0,minfinal=1/5)
  runstats(Td)
  return(Td)
} #generate 2d data set with normally distributed noise:each row has mean=0 (average over space)

findnonzerocolumns<-function(x){
  #xrange<-abs(min(c(x),na.rm = TRUE)-max(c(x),na.rm = TRUE))
  yrange<-NULL
  for(c in 1:ncol(x)){yrange<-c(yrange,abs(min(x[,c],na.rm = TRUE)-max(x[,c],na.rm = TRUE)))}
  xrange<-max(yrange)
  nonzero<-which(yrange/xrange>=0.05)
  ze<-which(yrange/xrange<0.05)
  return(nonzero)
}  #find all columns that have at least 1/20 the variaiton of the max vARIATION COLUMN

plot_vsref<-function(d,ref,
                     main="Data plots", xlab="Data Value (Reference)",limits=NULL,
                     lty="p",legf=FALSE,cex.main=0.8){
  if(length(d[,ref])<18 && legf) colourevent<-c((1:length(d[,ref]))+9) else colourevent<-"black"
  ref<-as.numeric(ref)
  #min(d,na.rm=TRUE)
  if(is.null(limits)){
   #  xlimits<-c(min(d[,ref],na.rm=TRUE),max(d[,ref],na.rm=TRUE))
   # ylimits<-c(min(d,na.rm=TRUE),max(d,na.rm=TRUE))
   mins<-min(d,na.rm = TRUE)
   maxs<-max(d,na.rm = TRUE)
   ds<-maxs-mins
   xlimits<-ylimits<-c(mins-1*ds*3/8,maxs+ds*3/8)
  }
  else{
    ylimits<-xlimits<-limits
  }
  plot(d[,ref],d[,ref],ylim=ylimits,xlim=xlimits , ylab="Data Value",xlab=xlab, lwd=3,cex=1.5,col=colourevent)
  title(main=main,cex.main=cex.main)
  apply(as.data.frame(seq(1,ncol(d), by=1)),1, FUN=function(v,d,ref){ lines(d[,ref],d[,v],type=lty, pch=v%%25,col=v) },ref,d=as.data.frame(d))

  if(legf)legend("bottomright",inset= 0.0,(rownames(d)), fill=colourevent )
} # plot all columns od d versus "ref" column : all straight lines if exactly equitable(with respect to the reference)


plotsquares<-function(Ta, num=5,signal=NULL,xlimits=NULL,slimits=NULL,blimits=NULL,indiv=FALSE,columns=FALSE,
                      images=TRUE,transpose=FALSE,density=FALSE,main="",psf=TRUE,
                      of=FALSE,lf=FALSE,ef=TRUE,errb=FALSE,row_unit=NULL,col_unit=NULL,z_unit=NULL,yline=3,yma=5,
                      stderror=FALSE){
  if(is.null(z_unit)){
    z_unit<-"Data Value"
    bz_unit<-paste("Intercept")
  } else {
    bz_unit<-paste("Intercept",z_unit)
  z_unit<-paste("Slope",z_unit)

  }
  row_unit=col_unit    #for matrices unit is column unit from data set
  if(!is.null(signal)){
    orig<-signal$l.s.s
  origb<-signal$l.s.b
  } else{
    orig<-Ta$l.s.s
    origb<-Ta$l.s.b
  }
  Eerr=NULL; lserr=NULL; Eaveerr=NULL
  Eerrb=NULL; lserrb=NULL; Eaveerrb=NULL
  if(transpose){
    o<-t(orig); E<-t(Ta$ET.Es);  ls<-t(Ta$l.s.s) ;ge<-"Transposed "    #ET.Es is masked version versus E.s
    ob<-t(origb) ;Eb<-t(Ta$ET.Eb);  lb<-t(Ta$l.s.b)
    if(!is.null(col_unit))y_unit<-paste("MATRIX ROWS ",col_unit) else y_unit<-"ROWS"
    if(!is.null(row_unit))x_unit<-paste("MATRIX COLUMNS ",row_unit) else x_unit<-"COLUMNS"

    if(errb){
      Eerr=t(Ta$E.sd1); lserr=t(Ta$l.s.sse)
      Eerrb=t(Ta$E.bsd1); lserrb=t(Ta$l.s.bse); Eaveerr=NULL
      if(stderror){
        Eerr<- Eerr/sqrt(t(Ta$E.sN)) ;
        ge<-paste0(g," std. error of Mean")
      }
    }
  } else {
    o<-orig; E<-Ta$ET.Es;  ls<-Ta$l.s.s ;ge<-" "
    ob<-(origb) ;Eb<-(Ta$ET.Eb);  lb<-(Ta$l.s.b)
    if(!is.null(col_unit))x_unit<-paste("MATRIX ROWS ",col_unit) else x_unit<-"ROWS"
    if(!is.null(row_unit))y_unit<-paste("MATRIX COLUMNS ",row_unit) else y_unit<-"COLUMNS"


    if(errb){
      Eerr=(Ta$E.sd1); lserr=(Ta$l.s.sse)
      Eerrb=(Ta$E.bsd1); lserrb=(Ta$l.s.bse); Eaveerrb=NULL
      if(stderror){
        Eerr<- Eerr/sqrt((Ta$E.sN)) ;
        Eerrb<- Eerrb/sqrt((Ta$E.bN)) ;
        ge<-paste0(ge," std. error of Mean")
      }
    }
  }

  if(is.null(slimits)){

    if(errb)szlimits<-c(mean(c(E-Eerr),na.rm = TRUE)-4*sd(c(E-Eerr),na.rm = TRUE),mean(c(E+Eerr),na.rm = TRUE)+4*sd(c(E+Eerr),na.rm = TRUE))
    else szlimits<-c(mean(c(E),na.rm = TRUE)-4*sd(c(E),na.rm = TRUE),mean(c(E),na.rm = TRUE)+4*sd(c(E),na.rm = TRUE))
    iszlimits<-c(0, 2)
    }else{
      szlimits<-slimits
      iszlimits<-slimits
  }

  if(is.null(blimits)){
    if(errb)bzlimits<-c(mean(c(Eb-Eerrb),na.rm = TRUE)-4*sd(c(Eb-Eerrb),na.rm = TRUE),mean(c(Eb+Eerrb),na.rm = TRUE)+4*sd(c(Eb+Eerrb),na.rm = TRUE))
    else bzlimits<-c(mean(c(Eb),na.rm = TRUE)-4*sd(c(Eb),na.rm = TRUE),mean(c(Eb),na.rm = TRUE)+4*sd(c(Eb),na.rm = TRUE))
    me<-mean(Eb,na.rm = TRUE)
    if(me<0){mi<-me;ma<-(-1)*me} else{ mi<- (-1)*me; ma<-me}
    ibzlimits<-c(mi,ma)
    ibzlimits<-c(mean(Eb,na.rm = TRUE)-1*sd(Eb,na.rm = TRUE),mean(Eb,na.rm = TRUE)+1*sd(Eb,na.rm = TRUE))
    #cat(ibzlimits)
    } else{
      bzlimits<-blimits
      ibzlimits<-blimits
    }


  if(is.null(xlimits))xlimits<-c(1,nrow(o))
  if(psf){
                             #plor squares needs a signal input corresponding to the l.s. fits
  if(images)plotimages(o,E,ls,zlimits=iszlimits,genname=paste(ge,"Slopes"),of=of,lf=lf,ef=ef,row_unit=y_unit,col_unit=x_unit,yma=yma,yline=yline)
  if(columns)plotO_S_E_lscol(o,E,ls,xlimits=xlimits,ylimits=szlimits,genname=paste(main,ge,"Slopes"),of=of,lf=lf,ef=ef,x_unit=x_unit,y_unit=z_unit)

  if(indiv){
    plotindivid(o,E,ls,xlimits=xlimits,ylimits=szlimits,num=num,genname=paste(main,ge,"Slopes"),of=of,lf=lf,ef=ef,err=errb,
                       Eerr=Eerr, lserr=lserr, Eaveerr=Eaveerr,x_unit=x_unit,y_unit=z_unit)
    vv<-seq(xlimits[1],xlimits[2], by=(xlimits[2]-xlimits[1])/100)
    lines(vv,rep(1,length(vv)))
  }
  }
  # zlimits<-ylimits<-c(mean(ob,na.rm = TRUE)-3/4*sd(ob,na.rm = TRUE),mean(ob,na.rm = TRUE)+3/4*sd(ob,na.rm = TRUE))
  if(images)plotimages(ob,Eb,lb,zlimits=ibzlimits,genname=paste(ge,"Intercepts"),of=of,lf=lf,ef=ef,row_unit=y_unit,col_unit=x_unit,yma=yma,yline=yline)
  if(columns)plotO_S_E_lscol(ob,Eb,lb,xlimits=xlimits,ylimits=bzlimits,genname=paste(main,ge,"Intercepts"),of=of,lf=lf,ef=ef,x_unit=x_unit,y_unit=bz_unit)

  if(indiv){
    plotindivid(ob,Eb,lb,xlimits=xlimits,ylimits=bzlimits,num=num,genname=paste(main, ge,"Intercepts"),of=of,lf=lf,ef=ef,err=errb,
                       Eerr=Eerrb, lserr=lserrb, Eaveerr=Eaveerrb,x_unit=x_unit,y_unit=bz_unit)
    vv<-seq(xlimits[1],xlimits[2], by=(xlimits[2]-xlimits[1])/100)
    lines(vv,rep(0,length(vv)))
  }
  if(density){
  if(of)  plotdensity(o,num=num,genname="Signal Slope")
 if(lf) plotdensity(ls,num=num,genname="Least Squares Slope")
 if(ef) plotdensity(E,num=num,genname="Equitable Slope")
  }
} # plot slopes and intercepts of matrices in various ways dependent on flags-error bars possible

#' Various types of plots of Equitable transform data dependent on options chosen
#'
#' Uses functions plotimages plotindiv plotO_S_E_lscol plotversus plot_vsref.
#' The function plotsummary uses this function to summarize the transform data
#'
#' @param T  equitable transform info: output from transformE
#' @param signal 2D data set representing the signal. Must be the same size as T$smat
#' @param images default TRUE: plots false colour images
#' @param indiv default FALSE: plots individual row or column dependendent on transpose flag
#' @param versus plots all individual vs reference default FALSE:
#' @param fcontour default TRUE: plots contour maps
#' @param ef  according to above flags plots equitable transform data default FALSE
#' @param lf  according to above flags plots least squares transformdata default FALSE
#' @param of  according to above flags plots original data default FALSE
#' @param avef  according to above flags plots equitbale transform data formed using only average profile default FALSE
#' @param errb  according to above flags uses error bars when possible default FALSE
#' @param xvsref column to be used as reference against which all others are plotted default NULL
#' @param row_unit row axis label  default Row Number
#' @param col_unit column axis label  default Row Number
#' @param z_unit  label  for quantity measured in data
#' @param genname  main title to be included
#' @param stderror according to above flags uses error bars of standard error rather than standard deviaiton when possible default FALSE
#' @param num  5 default number of indiviual plot to be made
#' @param limits default NULL limits on yaxis of plots and range for rainbow colouring in images
#' @param xlimits default NULL limits on xaxis of plots
#' @param columns  default FALSE TRUE:plot all columns on one plot
#' @param transpose  default FALSE TRUE: plot all rows on one plot
#' @param yline default 3 lines out from plot to display yaxis values
#' @param yma default 5 cahracters out from plot to display ylabel
#' @param density ignore
#'
#' @return None
#'
#' @examples
#' d<-eg4(2,2)
#' Td<-transformE(d)
#' d_noise<-d+rnorm(prod(dim(d)),mean=0,sd=(1/4*sd(d,na.rm=TRUE)))
#' Td_noise<-transformE(d_noise)
#' plotsome(T=Td_noise,lf=TRUE,of=TRUE)
#' # default only plots imagef of signal original and equitable
#' plotsome(T=Td_noise,signal=Td$smat,of=TRUE)
#' plotsome(T=Td_noise,signal=Td$smat,indiv=TRUE,xvsref=ncol(Td$smat))
#' plotsome(T=Td_noise,signal=Td$smat,columns=TRUE,images=FALSE,
#' lf=TRUE,of=TRUE)
#' plotsome(T=Td_noise,signal=Td$smat,columns=TRUE,images=FALSE,
#' transpose=TRUE,lf=TRUE,of=TRUE)
#' plotsome(T=Td_noise,signal=Td$smat,indiv=TRUE,of=TRUE,lf=TRUE)
#' plotsome(T=Td_noise,signal=Td$smat,indiv=TRUE,of=TRUE,errb=TRUE)
#' plotsome(T=Td_noise,signal=Td$smat,indiv=TRUE,of=TRUE,errb=TRUE,stderror=TRUE)
#' #num= NULL  all individuals are plotted
#' plotsome(T=Td_noise,signal=Td$smat,indiv=TRUE,of=TRUE,num=NULL)
#' # plots images and transforms vs original and signal
#' plotsome(T=Td_noise,signal=Td$smat,of=TRUE,lf=TRUE,versus=TRUE)
#'
#'
#' @export
plotsome<-function(T,num=5,signal=NULL,limits=NULL,xlimits=NULL,
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
}# plot  data sets in verious ways dependent on flags-0error bars std andse possible

plotdensity<-function(s,
                      genname="Slope",num=NULL ){

 # if(is.null(ylimits)) ylimits<-c(min(c(I,orig,E,ls),na.rm=TRUE),max(I,na.rm=TRUE))
  if(is.null(num))num<-ncol(I)-1
  if(length(num)==1)listx<-seq(1,ncol(I), by=(ncol(I)-1)/num) else listx<-num
  cat(listx)
  sapply(listx,function(c){breaks<-100; mu<-summary(abs(s[c,]),na.rm=TRUE); msd<- sd(abs(s[c,]),na.rm=TRUE);
  mh<-hist(abs(s[c,]),prob=1,breaks=breaks,xlim=c(0,10),main=paste(genname,": row=",c),xlab="Slope value",ylim=c(0,1.5));
  den<-density(as.vector(abs(s[c,])),na.rm=TRUE)
  lines(den,col=2,lty=1 ,lwd=2)
  lines(rep( mu[2],200), seq(0.01,2, by=0.01),lty=4 ,lwd=2)
  lines(rep( mu[3],200), seq(0.01,2, by=0.01),lty=4 ,lwd=4)
  lines(rep( mu[5],200), seq(0.01,2, by=0.01),lty=4 ,lwd=2);
  lines(rep( mu[4],200), seq(0.01,2, by=0.01),lty=1 ,lwd=4)})

}# plot  densities of slope values for reference all reference rows c:density of s[c,]

plotimages<-function(orig,E,ls,row_unit=NULL,col_unit=NULL,
                     signal=NULL,Eave, genname=NULL,zlimits=NULL, of=FALSE,lf=FALSE,ef=TRUE,yline=3,yma=5,
                     avef=FALSE,fcontour=TRUE){

if(is.null(zlimits)) zlimits<-c(min(E,na.rm=TRUE),max(E,na.rm=TRUE))
if(!is.infinite(zlimits[1])){
if(zlimits[2]<= zlimits[1]+1e-6)zlimits<-c(zlimits[1]-1/10,zlimits[1]+1/10)   #;cbflag<-FALSE} else cbflag<-TRUE


if(is.null(genname))genname<-"Data"
if(fcontour) {
if(of && sd(orig,na.rm=TRUE)>1e-6)contour(orig,main=paste0("Original",genname), zlim=zlimits/1,xlab=row_unit,ylab=col_unit)
if(!is.null(signal) && sd(signal,na.rm=TRUE)>1e-6)contour(signal,main=paste0("No Noise Signal: ",genname), zlim=zlimits/1,xlab=row_unit,ylab=col_unit)

if(ef && sd(E,na.rm=TRUE)>1e-6 )contour(E,main=paste0("T Noise Equitable",genname), zlim=zlimits/1,xlab=row_unit,ylab=col_unit)
if(avef && sd(Eave,na.rm=TRUE)>1e-6)contour(Eave,main=paste0("T Noise Equitable REFERENCED ",genname), zlim=zlimits/1,xlab=row_unit,ylab=col_unit)
if(lf && sd(ls,na.rm=TRUE)>1e-6)contour(ls,main=paste0("T Noise Least squared: ",genname), zlim=zlimits/1,xlab=row_unit,ylab=col_unit)
}
if(of)imagenan(orig,main=paste0("Original: ",genname), zlim=zlimits/1,row_unit=row_unit,col_unit=col_unit,yma=yma,yline=yline)
if(!is.null(signal))imagenan(signal,main=paste0("No Noise Signal: ",genname), zlim=zlimits/1,row_unit=row_unit,col_unit=col_unit,yma=yma,yline=yline)

if(ef)imagenan(E,main=paste0("T Noise Equitable : ",genname), zlim=zlimits/1,row_unit=row_unit,col_unit=col_unit,yma=yma,yline=yline)
if(avef)imagenan(Eave,main=paste0("T Noise Equitable REFERENCED : ",genname), zlim=zlimits/1,row_unit=row_unit,col_unit=col_unit,yma=yma,yline=yline)
if(lf)imagenan(ls,main=paste0("T Noise Least squared: ",genname), zlim=zlimits/1,row_unit=row_unit,col_unit=col_unit,yma=yma,yline=yline)
} else cat("\n ERROR: images transform is all nan")
} #plot images of original data (orig), Equitable transform (E) and least squares transform (ls)

plotO_S_E_lscol<-function(orig,E,ls,x_unit=NULL,y_unit=NULL,
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
}#plot all columns of original data (orig), Equitable transform (E) and least squares transform (ls)

plot_columns<-function(d,x_unit="INDEX",y_unit="DATA VALUE",
                       main="Data plots", limits=NULL,
                       xlimits=NULL){
  #min(d,na.rm=TRUE)
  if(is.null(limits)) limits<-c(min(d,na.rm=TRUE),max(d,na.rm=TRUE))
  if(is.null(xlimits)) xlimits<-c(1,nrow(d))
  #par(fig=c(0.,0.8,0.,0.8), new=FALSE)
  par(mfrow=c(1,1))
  plot(d[,1],ylim=limits,xlim=xlimits, main=main,xlab=x_unit, ylab=y_unit,xaxt='n',yaxt='n')
  if(is.null(y_unit)) y_unit<-"Data Value"
  if(is.null(x_unit)) x_unit<-"Index"
  rtick<-1
  er<-nrow(d)
  rtickinc<-(er-rtick)/10
  rnames<-rownames(d)
  axis(1,at=c(seq(rtick,er,by= rtickinc)),labels=rnames[seq(rtick,er,by=rtickinc)],lwd=2)
  axis(2,ylim= limits,lwd=2)
  #legend("topright", legend = paste0("Slopes at Reference ", cnames[j]))
  apply(as.data.frame(seq(ncol(d),1, by=(-1))),1, FUN=function(v){ lines(d[,v],type="o", pch=v%%25,col=v) })
  return(limits)
}#plot the row variations for all columns of d

plot_columnsnum<-function(d,
                          main="Data plots", xlab="Index",limits=NULL,
                          xlimits=NULL){
  #min(d,na.rm=TRUE)
  if(is.null(limits)) limits<-c(min(d,na.rm=TRUE),max(d,na.rm=TRUE))
  if(is.null(xlimits)) xlimits<-c(1,ncol(d))
  plot(d[1,],ylim=limits,xlim=xlimits, main=main, ylab="Data Value",xlab=xlab)

  apply(as.data.frame(seq(2,nrow(d), by=1)),1, FUN=function(v){ lines(d[v,],type="o", pch=paste0(v%%25),col=v) })  # paste0(v%%10)
  return(limits)
}#plot all columns d???

plotversus<-function(orig,E,ls,
                     signal=NULL,Eave,ylimits=NULL,genname=NULL, of=FALSE,lf=FALSE,ef=TRUE,
                     avef=FALSE) {
  if(is.null(genname))genname<-"Data"
  if(is.null(ylimits)) ylimits<-c(min(E,na.rm=TRUE),max(E,na.rm=TRUE))
  if(is.null(signal)){   #null signal then plot orig vs others
    if(of){

      if(lf){
        plot(ls,orig, main="Noise: Original vs Least Squares Transform ",xlab="Least Squares Transform",ylab="Original Data", ylim=ylimits,xlim= ylimits)
        lines(seq(ylimits[1],ylimits[2],by = (ylimits[2]-ylimits[1])/100 ),seq(ylimits[1],ylimits[2],by = (ylimits[2]-ylimits[1])/100 ))
      }
      if(ef){
        plot(E,orig,main="Noise: Original vs Equitable Transform ",xlab="Equitable Transform",ylab="Original Data",ylim=ylimits,xlim= ylimits)
        lines(seq(ylimits[1],ylimits[2],by = (ylimits[2]-ylimits[1])/100 ),seq(ylimits[1],ylimits[2],by = (ylimits[2]-ylimits[1])/100 ))
      }
      if(avef){
        plot(Eave,orig,main="Noise: Original vs Equitable Transform REFERENCED",xlab="Equitable Transform (Referenced)",ylab="Original Data",ylim=ylimits,xlim= ylimits)
        lines(seq(ylimits[1],ylimits[2],by = (ylimits[2]-ylimits[1])/100 ),seq(ylimits[1],ylimits[2],by = (ylimits[2]-ylimits[1])/100 ))
      }
    }
  } else {
      if(of){
      plot(signal,orig,main="Noise: Original Data vs Signal",xlab="Signal",ylab="Original Data",ylim=ylimits,xlim= ylimits)
lines(seq(ylimits[1],ylimits[2],by = (ylimits[2]-ylimits[1])/100 ),seq(ylimits[1],ylimits[2],by = (ylimits[2]-ylimits[1])/100 ))
}
if(lf){
  plot(signal,ls,main="Noise: Least Squares Transform vs Signal",xlab="Signal",ylab="Least Squares Transform",ylim=ylimits,xlim= ylimits)
lines(seq(ylimits[1],ylimits[2],by = (ylimits[2]-ylimits[1])/100 ),seq(ylimits[1],ylimits[2],by = (ylimits[2]-ylimits[1])/100 ))

plot(orig,ls,main="Noise: Least Squares Transform vs Original ",xlab="Original Data",ylab="Least Squares Transform",ylim=ylimits,xlim= ylimits)
lines(seq(ylimits[1],ylimits[2],by = (ylimits[2]-ylimits[1])/100 ),seq(ylimits[1],ylimits[2],by = (ylimits[2]-ylimits[1])/100 ))
}
if(ef){
  plot(signal,E,main="Noise: Equitable Transform vs Signal",xlab="Signal",ylab="Equitable Transform",ylim=ylimits,xlim= ylimits)
lines(seq(ylimits[1],ylimits[2],by = (ylimits[2]-ylimits[1])/100 ),seq(ylimits[1],ylimits[2],by = (ylimits[2]-ylimits[1])/100 ))

plot(orig,E,main="Noise: Equitable Transform vs Original ",xlab="Original Data",ylab="Equitable Transform",ylim=ylimits,xlim= ylimits)
lines(seq(ylimits[1],ylimits[2],by = (ylimits[2]-ylimits[1])/100 ),seq(ylimits[1],ylimits[2],by = (ylimits[2]-ylimits[1])/100 ))
}
    if(avef){
      plot(signal,Eave,main="Noise: Equitable Transform REFERENCED vs Signal",xlab="Signal",ylab="Equitable Transform REFERENCED",ylim=ylimits,xlim= ylimits)
      lines(seq(ylimits[1],ylimits[2],by = (ylimits[2]-ylimits[1])/100 ),seq(ylimits[1],ylimits[2],by = (ylimits[2]-ylimits[1])/100 ))
    }
  }
}# plot Equitable transform (E) and least squares transform (ls) versus original(or signal) data (orig)

plotindivid<-function(I,zw,lsz,
                      num=NULL,signal=NULL,Eave=NULL,xlimits=NULL,ylimits=NULL,genname=NULL, of=FALSE,lf=FALSE,ef=TRUE,avef=FALSE,err=FALSE,
                      Eerr=NULL, lserr=NULL,x_unit=NULL,y_unit=NULL,
                      Eaveerr=NULL ){

  #if(is.null(genname))genname<-"Data"
  if(is.null(ylimits)) ylimits<-c(min(zw,na.rm=TRUE)/1.0,max(zw,na.rm=TRUE)*1.0)
  if(is.null(xlimits)) xlimits<-c(1,nrow(zw))
  #cat("\nPlotindiv:xlimits: ", xlimits)
  #mu<-colMeans(I,na.rm = TRUE)
  if(is.null(num))num<-(ncol(I)-1)
  if(length(num)==1)listx<-seq(1,ncol(I), by=(ncol(I)-1)/num) else {
  if(length(num)==2 && num[1]==num[2])listx<-c(num[1]) else listx<-num
  }
  #cat("\nPlotindiv:Columns plotted ",listx," transpose=  :",transpose,"\n")
  #cat("\n",x_unit,y_unit)
  rnames<-rownames(I)
  for(y in listx){
    y<-floor(y)
    yn<-colnames(I)[y]
   if(ef){
     if(err){
       plotdata_with_errors(zw[,y],Eerr[,y],
                                  xlab=x_unit,ylab=paste(y_unit,yn),rnames=rnames,
                                  xlim=xlimits, main=paste("Index shown=",yn,"\nIncludes Error",genname),cex.main=0.7, ylim=ylimits, pch=11)
       } else {
                                    plot(zw[,y],xlab=x_unit,ylab=paste(y_unit,yn),
                          xlim=xlimits,ylim=ylimits, type="p",pch=11,main=paste("Index shown ",yn,"\n",genname),cex.main=0.7)

                                    }
    } else {
     if(of && !lf &&  !ef && !avef){
       plot(I[,y],xlab=x_unit,ylab=paste(y_unit,yn),main=paste("Index shown ",yn,"\n",genname),cex.main=0.7,
                            xlim=xlimits, ylim=ylimits, type="p",pch=15)
     } else {
      if(lf){
        plotdata_with_errors(lsz[,y],lserr[,y],xlab=x_unit,ylab=paste(y_unit,yn),rnames=rnames,xlim=xlimits,
                            main=paste("Index shown ",yn,"\n",genname),cex.main=0.7 ,ylim=ylimits, pch=1)
      } else {
        plotdata_with_errors(Eave[,y],Eaveerr[,y],xlab=x_unit,ylab=paste(y_unit,yn),rnames=rnames,
                               main=paste("Index shown ",yn,"\n",genname),cex.main=0.7 , xlim=xlimits,   ylim=ylimits, pch=0)
        }
      }
    }
   # plot(x[1,],pch=0)
    pch<-t((c(NA,NA,NA,NA,NA)))
    #pch<-t((c(15,24,11,0,NA)))
    #legend<-c('Original','Equitable','Least Squared','From Average','Signal')
    #lwd<-c(NA,NA,NA,NA,4)
    legend<-c(NA,NA,NA,NA,NA)
    lwd<-c(NA,NA,NA,NA,NA)
    if(!is.null(signal)){ legend[5]<-'Signal';lwd[5]<-4}
    if(of){legend[1]<-'Original'; pch[1]<-15}
    if(ef){legend[2]<-'Equitable'; pch[2]<-11}
    if(lf){legend[3]<-'Least Squared'; pch[3]<-1}
    if(avef){legend[4]<-'Eq. from Ave col'; pch[4]<-0}
    legend('topleft',inset=.02,legend=legend,
           lwd=lwd,pch = pch,bg='white',ncol=c(2),cex=0.75)

    if(of)lines(I[,y], type="p",pch=15);
    #lines(rep(mu[y],151), type="l",pch=11)
    if(!is.null(signal))lines(signal[,y], type="l",pch=15,lwd=4);
    if(lf)lines(lsz[,y], type="p",pch=1,lwd=2)
    if(avef)lines(Eave[,y], type="p",pch=0,lwd=2)

  }
}# plot original(orig) and/or signal, Equitable transform (zw) and least squares transform (lsx)

plotdata_with_errors<- function( dataset,data_std,rnames=1:length(dataset),
                                 main="data",ylim=c(0,1),xlim=NULL,xlab="ROW",ylab="DATA VALUE",
                                 pch="O",type="p",col="black",lty=1,lineonly=FALSE,cex.main=1){

  #dataset and data_std are the data vector and error bars respectively
  numrows <- length(dataset)
  if(is.null(xlim))xlim<-c(1,numrows)
  d = data.frame(
    x  = c(1:numrows)
    , y  = dataset
    , xsd = data_std
  )

  if(is.null(ylab)) ylab<-"Data Value"
if(is.null(xlab)) xlab<-"Index"
  if(!lineonly){
    plot(d$x, d$y ,pch=pch, ylim= ylim,xlim= xlim,xlab=xlab, ylab=ylab,xaxt='n',yaxt='n',
         cex.lab=1.5, cex.axis=1.5,  cex.sub=1.5)
  with (
    data = d
    , expr = Hmisc::errbar(x, y, y+xsd, y-xsd, add=TRUE, pch=pch,type=type,col=col,lty=lty, cap=.01)
  )
  title(main=main,cex.main=cex.main)
  rtick<-xlim[1]
er<-xlim[2]
rtickinc<-round((er-rtick)/10)
if(rtickinc==0)rtickinc=1
axis(1,at=c(seq(rtick,er,by= rtickinc)),labels=rnames[seq(rtick,er,by=rtickinc)],lwd=2,cex.lab=1.5, cex.axis=1.5,  cex.sub=1.5)

axis(2,ylim= ylim,lwd=2,cex.lab=1.5, cex.axis=1.5,  cex.sub=1.5)
  } else {
  line(d$x, d$y )
  with (
    data = d
    , expr = Hmisc::errbar(x, y, y+xsd, y-xsd, add=TRUE, pch=pch,type=type,col=col,lty=lty, cap=.01)
  )
}

#legend("topright", legend = paste0("Slopes at Reference ", cnames[j]))
#
#   legend("topright", legend = paste0(vals$parameter," for ", cname))
#   lines(d$x,rep(0,numrows))
#   lines(d$x,rep(1,numrows))
  return()
}# plot with errors (either std dev or std error) original(orig) and/or signal, Equitable transform (zw) and least squares transform (lsx)

plotAveprofiles<-function(Td96=Td, main="",xlim=NULL,ylim=NULL,xlab="ROW",ylab="DATA VALUE"){
  if(length(which(colnames(Td96$smat)=="Row_Ave"))!=0 ){
  smat<-Td96$smat[,which(colnames(Td96$smat)!="Row_Ave")]
  Ex<-Td96$ET.x[,which(colnames(Td96$ET.x)!="Row_Ave")]
  Exsd<-Td96$ET.xsd[,which(colnames(Td96$ET.xsd)!="Row_Ave")]
  } else{
    smat<-Td96$smat
    Ex<-Td96$ET.x
    Exsd<-Td96$ET.xsd
  }
      if(is.null(ylim)){
        mi<-min(c(Td96$smat,Td96$ET.x),na.rm=TRUE)
        ma<-max(c(Td96$smat,Td96$ET.x),na.rm=TRUE)
        m0<-mi+0.2*(ma-mi)
        mf<-ma-0.2*(ma-mi)
        ylim<-c(m0,mf)
      }
  aveprofileO<-rowMeans(smat,na.rm=TRUE)     #  rowMeans(Td96$smat[,which(colnames(Td96$smat)!="Row_Ave")],na.rm=TRUE)
  sdprofileO<-NULL; NprofileO<-NULL
  for (r in 1: nrow(smat)){sdprofileO<-c(sdprofileO, sd(smat[r,],na.rm=TRUE) ) ; NprofileO<-c(NprofileO,length(which(!is.na(smat[r,]))))  }
  names(sdprofileO)<-names(NprofileO)<-names(aveprofileO)

  aveprofile<-rowMeans(Ex,na.rm=TRUE)
  sdprofile<-NULL; Nprofile<-NULL
  for (r in 1: nrow(Ex)){sdprofile<-c(sdprofile, sd(Ex[r,],na.rm=TRUE) ) ; Nprofile<-c(Nprofile,length(which(!is.na(Ex[r,]))))  }
  names(sdprofile)<-names(Nprofile)<-names(aveprofile)
  plotdata_with_errors( aveprofileO,sdprofileO,rnames=rownames(smat),
                        main=paste("Average Original (Std. Dev. from Direct Average) of\n",main),ylim=ylim,xlim=NULL,
                        xlab=xlab,ylab=ylab,pch=(15),cex.main=0.85)
  plotdata_with_errors( aveprofile,sdprofile,rnames=rownames(Ex),
                        main=paste("Average Equitable (Std. Dev. from Direct Average)\nCalc. from",main),ylim=ylim,xlim=NULL,
                        xlab=xlab,ylab=ylab,pch=(15),cex.main=0.85)
  if(length(which(colnames(Td96$smat)=="Row_Ave"))!=0 ){
  plotdata_with_errors( Td96$ET.x[,"Row_Ave"],Td96$ET.xsd[,"Row_Ave"],rnames=rownames(Ex),
                        main=paste("Average Equitable (Std. Dev. using Equitable Errors) \n",main),ylim=ylim,xlim=NULL,
                        xlab=xlab,ylab=ylab,pch=(15),cex.main=0.85)
  }

  plotdata_with_errors( aveprofileO,sdprofileO/sqrt(NprofileO),rnames=rownames(smat),
                        main=paste("Average Original (Std. Error of Mean from Direct Average) of\n",main),ylim=ylim,xlim=NULL,
                        xlab=xlab,ylab=ylab,pch=(15),cex.main=0.85)
  plotdata_with_errors( aveprofile,sdprofile/sqrt(Nprofile),rnames=rownames(Ex),
                        main=paste("Average Equitable (Std. Error of Mean from Direct Average) of\n",main),ylim=ylim,xlim=NULL,
                        xlab=xlab,ylab=ylab,pch=(15),cex.main=0.85)
  if(length(which(colnames(Td96$smat)=="Row_Ave"))!=0 ){
  plotdata_with_errors( Td96$ET.x[,"Row_Ave"],Td96$ET.xsd[,"Row_Ave"]/sqrt(Td96$ET.EN[,"Row_Ave"]),rnames=rownames(Td96$ET.x),
                        main=paste("Average Equitable (Std. Error of Mean using Equitable Errors) \n",main),ylim=ylim,xlim=NULL,
                        xlab=xlab,ylab=ylab,pch=(15),cex.main=0.85)
  }

  names(aveprofile)<-rownames(Td96$ET.x)
  # #plot(aveprofile,main=,xlab="Event Name",ylab="Event Average Day Number")
  # plot(1:length(aveprofile), aveprofile,xaxt="n",main=paste("Average Equitable profile of\n",avename,"\n",(sample))
  #      ,xlab="Event Name",ylab="Event Average Day Number",pch=(15),type="b",lty=2,ylim=ylim)   #
  # axis(1, at=seq(1,length(names(aveprofile)),by=1), labels=names(aveprofile)[seq(1,length(names(aveprofile)),by=1)])
  if(length(which(colnames(Td96$smat)=="Row_Ave"))!=0 ){
  profileinfo<-list(aveprofileO=aveprofileO,sdprofileO=sdprofileO,NprofileO=NprofileO,
                    aveprofile=aveprofile,sdprofile=sdprofile,Nprofile=Nprofile,
                    aveprofileE=Td96$ET.x[,"Row_Ave"],sdprofileE=Td96$ET.xsd[,"Row_Ave"],NprofileE=Td96$ET.EN[,"Row_Ave"])
  } else{
    profileinfo<-list(aveprofileO=aveprofileO,sdprofileO=sdprofileO,NprofileO=NprofileO,
                      aveprofile=aveprofile,sdprofile=sdprofile,Nprofile=Nprofile)
  }
  #0 is original data     aveprofile is Equitable average   aveprofileE is Equtiable point values with equitable errors
  return(profileinfo)
}#aveprofile0 is smat ave, aveprofile is average of Equitable Transform, aveprofileE is Equitbale trtansdform at "Row_Ave"

plotgroup_with_errors<-function(smatave,smatsd,main="",xlim=NULL,ylim=NULL,xlab="ROW",ylab="DATA VALUE",inames=NULL,
                                leg=TRUE,maxnum=8,cex=NULL,cex.main=0.85){
  if(is.null(cex))cex=0.8
  if(is.null(inames))inames<-colnames(smatave)

  for (firstnum in seq(1,ncol(smatave), by=maxnum)){

  if((firstnum+maxnum-1)<=ncol(smatave))endval<-(firstnum+maxnum-1) else endval<-ncol(smatave)
  plotdata_with_errors( smatave[,firstnum],smatsd[,firstnum],rnames=rownames(smatave),
                        main=paste(main,(firstnum-1)/maxnum),xlim=NULL,
                        xlab=xlab,ylab=ylab,ylim=ylim,pch=16,col=3,cex.main=cex.main)
  for (j in firstnum:endval){    #  for (j in 1:ncol(smatave)){ cat(" ",j%%5+1 ) }  for (j in 1:ncol(smatave)){ cat(" ",j,j%%5 ) }
    jp<-(j-firstnum+1)%%11
    jc<-(j-firstnum+1)
    jt<-(j-firstnum+1)%%5
    plotdata_with_errors( smatave[,j],smatsd[,j],rnames=rownames(smatave),
                          main=paste(main,(firstnum-1)/maxnum),xlim=NULL,xlab=xlab,ylab=ylab,ylim=ylim,
                          pch=(jp+15),type="b",col=(jc+2),lty=jt+1,lineonly=TRUE,cex.main=cex.main)
  }
  if(leg)legend("topleft",inset= c(0,0.0),paste(inames[firstnum:(endval)]),
                pch=(1:(endval-firstnum+1)%%11+15) ,
                col=(1:(endval-firstnum+1)+2) ,
                lty=(1:(endval-firstnum+1)%%5+1) ,cex=cex,pt.cex=1)

}
}


plotx_vs_y_with_errors<- function( x, y,stdy, xlab="Reference", ylab="non-Reference",main="Non-reference Vs Reference",
                                   ylim=c(140,260),rnames=names(y),
                                   pch=15,cex.main=0.8){
  #dataset and data_std are the data vector and error bars respectively
  numrows <- length(y)
  d = data.frame(
    x  = x
    , y  = y
    , sd = stdy
  )

  plot(d$x, d$y ,xlim= ylim,ylim= ylim,xlab=xlab,ylab=ylab)   #,main=main
  with (
    data = d
    , expr = Hmisc::errbar(x, y, y+sd, y-sd,add=TRUE,  pch=pch, cap=.01)
  )
 title(main=main,cex.main=cex.main)



  lines(ylim[1]:ylim[2],rep(0,length(ylim[1]:ylim[2])))
  lines(rep(0,length(ylim[1]:ylim[2])),ylim[1]:ylim[2])
  lines(ylim[1]:ylim[2],ylim[1]:ylim[2],lty=2)
  return()
}

#' Plots individual sequences against a reference sequence
#'
#' Equitable transform with errors, original and Equitable transform line parameters can be shown
#' If number of point in variable t is less than 26 then each t point is colured and labelled in the legend
#'
#' @param Td  transform information from transformE
#' @param cgroup  cgroup is the vector of columns to be plotted. Default NULL;10 colums across matrix are plotted
#' @param ref  reference column against which all columns are plotted. Default NULL; column index with largest variation
#' @param extranames extra names to use for legend of t points
#' @param err95 2*std. dev/sqrt(N) where std deviation on the slope is found when calculating equitable slopes  Default=TRUE
#' @param ylim vector of min and max for plots to display default (NULL) function calculates same for all
#' @param fitl Default TRUE. Fitted line is displayed
#' @param br text for headings of plots
#' @param pf plotflag default TRUE
#'
#' @return column index with largest variation amongst columns that were plotted
#'
#' @examples
#' #first construct transfor of data and transform of signal
#' d<-eg4(1,2)
#' d_noise<-d+rnorm(prod(dim(d)),mean=0,sd=(1/4*sd(d,na.rm=TRUE)))
#' Td_noise<-transformE(d_noise)
#' xvsrefplot(Td=Td_noise)
#' xvsrefplot(Td=Td_noise,ref="Row_Ave")
#' nc<-seq(1,ncol(Td_noise$smat), by=1)
#' xvsrefplot(Td=Td_noise,cgroup=nc,ref=ncol(Td_noise$smat),
#' br=paste0( "Equitable Profiles"))
#' xvsrefplot(Td=Td_noise,cgroup=c(1,4,9,12),ref=7,
#' br=paste0( "Equitable Profiles"))
#' xvsrefplot(Td=Td_noise,cgroup=c(1,4,9,12),ref=7,
#' br=paste0( "Equitable Profiles with std dev"),fitl=FALSE)
#' xvsrefplot(Td=Td_noise,cgroup=c(12),ref=7,
#' br=paste0( "Equitable Profiles"))
#' xvsrefplot(Td=Td_noise,cgroup=c(12),ref=7,
#' br=paste0( "Equitable Profiles"),err95=FALSE)
#'
#' d<-eg5(3,3)
#' d_noise<-d+rnorm(prod(dim(d)),mean=0,sd=(1/2*sd(d,na.rm=TRUE)))
#' Td_noise<-transformE(d_noise)
#' xvsrefplot(Td=Td_noise)

#'
#' @export
xvsrefplot<-function(Td,cgroup=NULL,ylim=NULL,ref=NULL,br="",pf=TRUE,extranames=NULL,err95=TRUE,fitl=TRUE){
  if(is.null(cgroup))cgroup<-seq(1,ncol(Td$smat),by=(ncol(Td$smat)-1)/10)

  if(!is.null(ref)){
    nref<-which(colnames(Td$smat)=="Row_Ave")[1]
    if( ref=="Row_Ave"){
 if(length(nref)!=0 )ref<-nref else ref<-NULL
    }
    cat("\nref set to ",ref)
  }
  #if(is.null(ref))ref<-ncol(Td$ET.x)
  if(is.null(ylim)){
    mins<-min(Td$smat[,cgroup],na.rm = TRUE)
    maxs<-max(Td$smat[,cgroup],na.rm = TRUE)
    ds<-maxs-mins
    if(nrow(Td$ET.x)<=21) ylim<-c(mins-1*ds*3/8,maxs+ds*3/8) else ylim<-c(mins-1*ds*3/8,maxs+ds*0/8)

    if((ylim[2]-ylim[1])<1e-10)ylim<-c(0,1)
    cat("\nylim set to ",ylim)
    }
   yrange<-NULL
   if(is.null(cgroup))cgr<-1:ncol(Td$ET.x) else cgr<-cgroup
    for(c in cgr){yrange<-c(yrange,abs(min(Td$ET.x[,c],na.rm = TRUE)-max(Td$ET.x[,c],na.rm = TRUE)))}
    ref1<-cgr[which(max(yrange,na.rm=TRUE)==yrange)]
    ref1<-ref1[1]
    if(is.null(ref1)){cat("\nref not set :initializing to 1");ref<-ref1<-1}
  if(!is.null(ref)) {
    refrange<-abs(min(Td$ET.x[,ref],na.rm = TRUE)-max(Td$ET.x[,ref],na.rm = TRUE))
    if(refrange/abs(ylim[2]-ylim[1])<0.1 ||is.null(ref))ref<-ref1
  } else { cat("\n reinitializing ref to ", ref1); ref<-ref1}
  rmean<-Td$ET.x[,ref]
  rmeano<-Td$smat[,ref]
  cat("\nref set to ",ref)
  for(c in cgroup) {
    #cat("\n",c)
    y<-Td$ET.x[,c]
    yo<-Td$smat[,c]
    #cat("\n",y)
    stdy<-Td$ET.xsd[,c]
    ps<-Td$l.s.pslope[c,ref]
    r2<-Td$l.s.r2[c,ref]
    s<-Td$E.s[c,ref]
    b<-Td$E.b[c,ref]

    if(err95){
    se<-2*Td$E.sd1[c,ref]/sqrt(Td$E.sN[c,ref])
    be<-2*Td$E.bsd1[c,ref]/sqrt(Td$E.bN[c,ref])
    sN<-Td$E.sN[c,ref]
    bN<-Td$E.bN[c,ref]
    } else {
      se<-NULL
      be<-NULL
      sN<-NULL
      bN<-NULL
    }
    #refname and cname are names of varianble that wiill be displayed to show both needs shrinkage of font
    if(!is.numeric(ref))gref<-which(colnames(Td$ET.x)==ref) else gref<-ref

    if(pf){
    if( fitl){
      if(is.null(extranames)) plotp1vsp2(rmean=rmean,rmeano=rmeano,y=y,yo=yo,stdy=stdy,ylim=ylim,ps=ps,r2=r2,s=s,b=b,
                                         se=se,be=be,sN=sN,bN=bN,
                                         br=paste0(br),
                                         refname=colnames(Td$ET.x)[gref],cname=colnames(Td$ET.x)[c]) else {
                                           plotp1vsp2(rmean=rmean,rmeano=rmeano,y=y,yo=yo,stdy=stdy,ylim=ylim,ps=ps,r2=r2,s=s,b=b,
                                                      se=se,be=be,sN=sN,bN=bN,
                                                      br=paste0(br),
                                                      refname=extranames[gref],cname=extranames[c])
                                         }
    } else {
      if(is.null(extranames)) plot1vs2(rmeano=rmeano,yo=yo,ylim=ylim,
                                         br=paste0(br),
                                         refname=colnames(Td$ET.x)[gref],cname=colnames(Td$ET.x)[c]) else {
                                           plot1vs2(rmeano=rmeano,yo=yo,ylim=ylim,
                                                      br=paste0(br),
                                                      refname=extranames[gref],cname=extranames[c])
                                         }
    }
  }
  }
  return(ref1)
}

plotp1vsp2<-function(rmean,rmeano,y,yo,stdy,ylim=NULL,ps=1,r2=0,
                     s=NULL,b=NULL,se=NULL,be=NULL,sN=NULL,bN=NULL,refname="unknown",cname="unknown",br=""){
 # lotscol<-colors()[c(24,94,26,130,121,96,97,49,47,417,256,8,33,90,142,144,653)]
  lotscol<-colors()[c(24,94,26,124,633,450,453,11,68,254,257,51,630,76,142,150,653)] #plot(1:length(lotscol),col=lotscol,pch=15,cex=4)
  lotscol<-c(lotscol,"darkorchid","darkkhaki","lightpink" ,"lightskyblue")
  if(length(unique(names(y)))<=length(lotscol) && length(unique(names(y)))>1){
    colourevent<-lotscol[ 1:length(unique(names(y)))]
    if(length(unique(names(yo)))<=8)colourevent<-c((1:length(y))+9)    # co<-1:length(unique(rownames(y)));
    if(length(unique(names(y)))!=length(y)){
    colourevent<-rep(NA,length(y))
    for(j in 1:length(unique(names(y)))){ colourevent[which(names(y)==unique(names(y))[j])]<-j  }
    }
  } else colourevent<-"black"
  plotx_vs_y_with_errors(x=rmean,y=y,stdy=stdy,ylim=ylim,
                         xlab=paste0("Ref. ",refname),ylab=paste0("Non-Ref. ",cname),
                         main=paste0(br,"\nNon-ref. ",cname,"\nVs Ref. ",refname),cex.main=0.7)
  points(rmeano,yo,
         pch=19,cex=1.5, col=colourevent)
  lines(rmean,y,lty=1)

  if(length(unique(names(y)))<25 && length(unique(names(y)))>1){

    if(length(unique(names(y)))!=length(y)){
      legend("bottomright",inset= 0.0,(unique(names(y))), fill=colourevent )
    } else legend("bottomright",inset= 0.0,(names(y)), fill=colourevent )
  }

  legend<-c("Equitable","Slope of 1","Original")
  lwd<-c(1,1,NA)
  lty<-c(1,2,NA)
  pch<-c(15,NA,19)

  legend('topleft',inset=.02,legend=legend,
         lwd=lwd,lty=lty,pch = pch,bg='white',ncol=c(2),cex=0.75)

  legend("bottomleft", legend = c(paste0("p= ",round(ps,digits=5) ),paste0("Coef.Det.= ",round(r2,digits=3) ) ))
  if(!is.null(s)&&!is.null(b) ){
    if(!is.null(se)&&!is.null(be) ) legend("bottom",
                                       legend = c(paste0("Slope= ",round(s,digits=2) ),paste0("(95% ",round(se,digits=2),") N=",sN ),
                                       paste0("Inter.= ",round(b,digits=1)),paste0("(95% ",round(be,digits=1),") N=",bN ) )) else
    legend("bottom", legend = c(paste0("Slope= ",round(s,digits=2) ),paste0("Intercept.= ",round(b,digits=1) ) ))
  }
}


plot1vs2<-function(rmeano,yo,ylim=NULL,
                     refname="unknown",cname="unknown",br=""){
  if(is.null(ylim)){
    mins<-min(c(yo,rmeano),na.rm = TRUE)
    maxs<-max(c(yo,rmeano),na.rm = TRUE)
    ds<-maxs-mins
    if(length(yo)<=21) ylim<-c(mins-1*ds*3/8,maxs+ds*3/8) else ylim<-c(mins-1*ds*3/8,maxs+ds*0/8)

    if((ylim[2]-ylim[1])<1e-10)ylim<-c(0,1)
  }
  # lotscol<-colors()[c(24,94,26,130,121,96,97,49,47,417,256,8,33,90,142,144,653)]
  lotscol<-colors()[c(24,94,26,124,633,450,453,11,68,254,257,51,630,76,142,150,653)] #plot(1:length(lotscol),col=lotscol,pch=15,cex=4)
  lotscol<-c(lotscol,"darkorchid","darkkhaki","lightpink" ,"lightskyblue")

  if(length(unique(names(yo)))<=length(lotscol) && length(unique(names(yo)))>1){
     colourevent<-lotscol[ 1:length(unique(names(yo)))]
     if(length(unique(names(yo)))<=8) colourevent<-c((1:length(yo))+9)    # co<-1:length(unique(rownames(yo)));
     if(length(unique(names(yo)))!=length(yo)){
      colourevent<-rep(NA,length(yo))
      for(j in 1:length(unique(names(yo)))){ colourevent[which(names(yo)==unique(names(yo))[j])]<-j  }
    }
  } else colourevent<-"black"
  plot(rmeano,yo, pch=19,cex=2, col=colourevent,ylim=ylim,xlim=ylim,
                         xlab=paste0("Ref. ",refname),ylab=paste0("Non-Ref. ",cname),
                         main=paste0(br,"\nNon-ref. ",cname,"\nVs Ref. ",refname),cex.main=1)
  lines(rmeano,rmeano,lty=1)
  # points(rmeano,yo,
  #        pch=19,cex=1.5, col=colourevent)
  # lines(rmean,yo,lty=1)

  if(length(unique(names(yo)))<25 && length(unique(names(yo)))>1){

    if(length(unique(names(yo)))!=length(yo)){
      legend("bottomright",inset= 0.0,(unique(names(yo))), fill=colourevent )
    } else legend("bottomright",inset= 0.0,(names(yo)), fill=colourevent )
  }

  }



make_data<-function(f,g,u,
                    cend,rend,rnum0,cnum0,rmult,
                    cmult,pf=TRUE, main=" "){
  #run below code for each example
  t<-1:(rnum0*rmult) ; x<-1:(cnum0*cmult)
  d<-outer(t,x,Vectorize(FUN=function(t,x,f,g,u){f[x]*g[t]+u[x]} )
           ,f=as.data.frame(f),u=as.data.frame(u),g=as.data.frame(g))
  rownames(d)<-t; colnames(d)<-x
  if(pf && nrow(d) >1 && ncol(d)>1)imagenan(d,main=paste0( main,"\nSIGNAL: rmult= ",rmult," cmult= ",cmult))
  return(d)
}#generate 2d data set based on vectors f g and u  whose size depends on cend and rend and resosolution rmult cmult

#' Summary plots of transforms
#'
#' Plots of various output from a transform: shows both Equitable and Least squares results
#' Will compare them to the original data and to a signal is it is available
#' Various formats for displaying the dat are used including images, contours and row/column plots with error bars showbn
#'
#' @param Td_noise Output from the transform function transformE fro the data to be studied
#' @param Td     NULL(Default)  Output from the transform program for an underlying signal. Allows comparisons with undelrlying signal
#' @param Td_old      ignore
#' @param row_unit     name for the row dimension for axis plotting e.g. "Day number"
#' @param col_unit     name for the row dimension for axis plotting e.g. "Year"
#' @param z_unit       name for the measured quantity e.g. "Temperature"
#' @param yline        3 (default) number of lines  from image to start ylabel
#' @param yma          5 (default) distance in from margin to start images
#' @param fintersect   FALSE (default)  TRUE: plots intercept vs 1-slope for different zeroes
#' @param fall         FALSE (default)  TRUE: all "events are used to construct bagplots when finterswect is also TRUE
#' @param fsquares     TRUE (default)   line p[lots] produced of slope and shift square matrices
#' @param fave         FALSE (default)  TRUE: shows results with errors of performing averaging ion data
#' @param inc          NULL(default) 10 colums plotted  : inc when set is the increment in columns that the plots step through
#'
#' @return None
#'
#' @examples
#' # first create a data set d and create the associated transforms.
#' # In this case d is eg7 with a resolution 3x higher than the lowest
#' #consider putting the graphs into a pdf file  by bracketing your
#' #commands beginning with pdf(file="foo.pdf) and ending with dev.off()
#' #(includes last column as average
#' # sequence profile : use Ave=FALSE to eliminate this column )
#' d<-eg7(3,3);Td<-transformE(d)
#' #when the data is perfectly equitable many plots
#' #are identical for the different transforms
#' plotsummary(Td)
#' #points (even for average profile) have no error
#' # in perfectly equitable system as they are specified by f,g, and u
#' plotsummary(Td,fave=TRUE)
#'                #add noise to this signal data set
#' #find the std dev of the overall signal and add normally distributed noise
#' sdd<-sd(d,na.rm=TRUE)
#' # that has a std. dev that is some fraction (fac) of this signal std dev
#' #set the fraction of noise relative to the standard deviaiton of the signal
#' fac<-1/3
#' #add to signal a normal distribution of noise with this std dev.
#' d_noise<-d+rnorm(prod(dim(d)),mean=0,sd=fac*sdd)
#' d_noise<-matrix(d_noise,nrow=nrow(d),ncol=ncol(d))
#' rownames(d_noise)<-rownames(d); colnames(d_noise)<-colnames(d)
#' Td_noise<-transformE(d_noise)  #transform the noisy data
#' #shows how the transform looks compared to the original data
#' plotsummary(Td_noise)
#' #shows how the data looks compared to the signal data
#' plotsummary(Td_noise,Td)
#' #change the label spacing on the images to fit in the yaxis numbers
#' plotsummary(Td_noise,yline=5,yma=8)
#' plotsummary(Td_noise,Td,yline=5,yma=10, fave=TRUE,
#'  row_unit="Day Number", col_unit="Year",
#'  z_unit="Temperature (C)",inc=1)
#'  # plot averages of the data /signal and
#'  #compare to averages with error due to equitable system
#' # 45x30 data set of 3 sets of random numbers coupled together
#' #in an equitable system
#' d<-eg8(3,3)
#' Td<-transformE(d,Ave=TRUE)
#' #data set entirely equitable but rows and column values have random distribution
#' plotsummary(Td_noise=Td,fave=TRUE)
#' # averages along rows and columns show large error but
#' #system is entirely specified by f,g,u
#' #no errors in knowing equitable average values as they are entiely
#' # specified in system
#'
#' @export
plotsummary<-function(Td_noise,Td=NULL,Td_old=NULL,
                      row_unit=NULL,col_unit=NULL,z_unit=NULL,
                      yline=3,yma=5,fintersect=FALSE,fsquares=FALSE,fave=FALSE,fall=FALSE,inc=NULL){
  if(!is.null(Td))runstatsNS(Td,Td_noise)
  plotsquares(Td_noise, signal=Td,of=TRUE,
              row_unit=row_unit,col_unit=col_unit,z_unit=z_unit) #Show images of slopes and intercepts
  if(fsquares){
  plotsquares(Td_noise, signal=Td,transpose=TRUE,images=FALSE,columns=TRUE,of=TRUE,
              row_unit=row_unit,col_unit=col_unit,z_unit=z_unit) #all rows plotted together

  plotsquares(Td_noise,signal=Td,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE,
              row_unit=row_unit,col_unit=col_unit,z_unit=z_unit) #with least sq, fits can put limits on,slimits=c(0,3) ,blimits=c(-10,+10)
  plotsquares(Td_noise,signal=Td,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE,stderror=TRUE,lf=TRUE,
              row_unit=row_unit,col_unit=col_unit,z_unit=z_unit) #with signal
  }
  plotsome(T=Td_noise,signal=Td$smat,transpose=TRUE,images=FALSE,indiv=TRUE,of=TRUE,lf=TRUE,errb=TRUE,
           row_unit=row_unit,col_unit=col_unit,z_unit=z_unit ) # could use vector num<-c(1,10,11,15)
  plotsome(T=Td_noise,signal=Td$smat,images=FALSE,indiv=TRUE,of=TRUE,lf=TRUE,errb=TRUE,
           row_unit=row_unit,col_unit=col_unit,z_unit=z_unit )
  plotsome(T=Td_noise,signal=Td$smat,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE ,stderror=TRUE,
           row_unit=row_unit,col_unit=col_unit,z_unit=z_unit) #plots individual columns of Equitable Transform with std error of mean at each point
  plotsome(T=Td_noise,signal=Td$smat,transpose=TRUE,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE ,stderror=TRUE,
           row_unit=row_unit,col_unit=col_unit,z_unit=z_unit)#plots individual rows of Equitable Transform with std error of mean at each point
#   plotsome(T=Td_noise,signal=Td$smat,num=c(132,123,99,92,77),transpose=TRUE,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE ,stderror=TRUE,row_unit=row_unit,col_unit=col_unit,z_unit=z_unit)#plots individual rows of Equ
  plotsome(T=Td_noise,signal=Td$smat,transpose=TRUE,images=FALSE,columns=TRUE,of=TRUE,lf=TRUE,
           row_unit=row_unit,col_unit=col_unit,z_unit=z_unit) #plots all columns together for original, least squares,signal and equitable
  plotsome(T=Td_noise,signal=Td$smat,images=FALSE,columns=TRUE,of=TRUE,lf=TRUE,row_unit=row_unit,
           col_unit=col_unit,z_unit=z_unit) #plots all columns together for original, least squares,signal and equitable
  stats_residuals(Td_noise,Td=Td,Td_old=Td_old,genname="Equitable",ylim=NULL,ipf=FALSE,pf=FALSE)
  if(is.null(inc)){
  inc<-round(ncol(Td_noise$smat)/10)  # inc<-2
  if(inc<1)inc<-1
  }
  nc<-seq(1,ncol(Td_noise$smat),by=inc)
  cat("\nnc")
  print(nc)
  la<-which(colnames(Td_noise$l.s.pslope)=="Row_Ave")    #Td$l.s.pslope[which(is.nan(Td$l.s.pslope))]<-NA
  if(length(la)!=0){pm<-colMeans(Td_noise$l.s.pslope[,-la],na.rm=TRUE)
  }  else pm<-colMeans(Td_noise$l.s.pslope,na.rm=TRUE)
  refer<-which(pm==min(pm,na.rm=TRUE))

  if(length(refer)>0)cat("\nbest reference individual is ",colnames(Td_noise$l.s.pslope)[refer],"\n") else{
    cat("\n no minp reference found: reset to 1\n")
    refer<-1
  }
  if(length(refer)>1) refer<-refer[1]
  xvsrefplot(Td=Td_noise,cgroup=nc,ref=refer,br=paste0( "Equitable Profiles with Min Probability Profile"))
  main<-paste("minp reference\n",colnames(Td_noise$l.s.pslope)[refer])
  plot_hist(Td_noise,refer=refer,main=main)
  # main<-paste("\nEntire Matrix")
  # plot_hist(Td_noise,main=main)

  if(length(la)==1)xvsrefplot(Td=Td_noise,cgroup=nc,ref="Row_Ave",br="Td_noise")   #compare profiles with either Row_Ave or  column with largest range
 # xvsrefplot(Td=Td_noise,cgroup=nc,ref=132,br="Td_noise")

  if(fintersect){
    if(length(la)!=0 )
    bestintersectname<-a_b_bagplot(community.f=NULL,
                                   Td=Td_noise,refindex=la,fall=fall) else bestintersectname<-a_b_bagplot(community.f=NULL,Td=Td_noise,refindex=1,fall=fall)
    }

  if(fave){
    plotAveprofiles(Td96=Td_noise, main="Td_Noise Data")
  if(!is.null(Td))plotAveprofiles(Td96=Td, main="Td Signal Data",xlab=row_unit,ylab=z_unit)
}
   plotsome(Td_noise,images=FALSE, xvsref=ncol(Td_noise$smat),
            row_unit=row_unit,col_unit=col_unit,z_unit=z_unit )  #plot all columns from Ave contructed transform versus the Average (reference column)
  plotsome(T=Td,images=FALSE, xvsref=ncol(Td$smat),
           row_unit=row_unit,col_unit=col_unit,z_unit=z_unit ) #plot all Signal columns from Ave contructed transform versus the Average (reference column)
  plotsome(T=Td_noise,signal=Td$smat,images=FALSE,versus=TRUE,of=TRUE,lf=TRUE,
           row_unit=row_unit,col_unit=col_unit,z_unit=z_unit)  #compares
#  plotsome(T=Td_noise,signal=Td$smat,images=FALSE,versus=TRUE,of=TRUE,lf=TRUE,limits=row_unit, row_unit=row_unit,col_unit=col_unit,z_unit=z_unit)  #compares
 #could put in error bars for versuws type plots
  plotsome(T=Td_noise, signal=Td$smat,of=TRUE,lf=TRUE,errb=TRUE,
           row_unit=row_unit,col_unit=col_unit,z_unit=z_unit,yma=yma,yline=yline)     #images and contours of Equitable transform
}
#example using particular columns


plotsum4<-function(T4,row_unit=NULL,col_unit=NULL,z_unit=NULL,fpca=FALSE){

  cmult<-T4$cmult
  rmult<-T4$rmult
  fac<-T4$fac
  NAfrac<-T4$NAfrac
  Td<-T4$Td
  TdNA<-T4$TdNA
  Tdn<-T4$Tdn
  TdnNA<-T4$TdnNA
  if(!is.null(TdnNA)){
  plotsome(TdnNA,images=FALSE,signal=Td$smat,indiv=TRUE,of=TRUE,errb=TRUE,row_unit=row_unit,col_unit=col_unit,z_unit=z_unit )
  plotsome(TdnNA,images=FALSE,signal=Td$smat,transpose=TRUE,indiv=TRUE,of=TRUE,errb=TRUE,row_unit=row_unit,col_unit=col_unit,z_unit=z_unit )
  plotsome(TdnNA,signal=Td$smat,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE ,stderror=TRUE,row_unit=row_unit,col_unit=col_unit,z_unit=z_unit)#plots individual columns of Equitable Transform with std error of mean at each point
  plotsome(TdnNA,signal=Td$smat,transpose=TRUE,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE ,stderror=TRUE,row_unit=row_unit,col_unit=col_unit,z_unit=z_unit)#plots
  plotsome(TdnNA,signal=Td$smat,images=FALSE,versus=TRUE,of=TRUE,lf=TRUE,row_unit=row_unit,col_unit=col_unit,z_unit=z_unit)  #compares
  plotsome(TdnNA,signal=Td$smat,images=TRUE,of=TRUE,lf=TRUE,row_unit=row_unit,col_unit=col_unit,z_unit=z_unit)  #compares
  stats_residuals(TdnNA,Td=Td,genname="Equitable",ylim=NULL,ipf=FALSE,pf=FALSE)
  inc<-round(ncol(TdnNA$smat)/10)  # inc<-2
  if(inc<1)inc<-1
  nc<-seq(1,ncol(TdnNA$smat),by=inc)

  la<-which(colnames(TdnNA$l.s.pslope)=="Row_Ave")
  if(length(la)!=0){pm<-colMeans(TdnNA$l.s.pslope[,-la],na.rm=TRUE)
  }  else pm<-colMeans(TdnNA$l.s.pslope,na.rm=TRUE)
  refer<-which(pm==min(pm))
  cat("\nbest reference individual is ",colnames(TdnNA$l.s.pslope)[refer],"\n")
  if(length(refer)>1) refer<-refer[1]
  xvsrefplot(Td=Tdn,cgroup=nc,ref=refer,br=paste0( "Equitable Profiles with Min Probability Profile"))
  main<-paste("minp reference\n",colnames(TdnNA$l.s.pslope)[refer])
  plot_hist(TdnNA,refer=refer,main=main)

  main<-paste("\nEntire Matrix")
  plot_hist(TdnNA,main=main)

  if(NAfrac<0.76){
    findinfo(Tdave=TdnNA,printmax=FALSE,numb=1)   #if too many missing then uit fails
    bestintersectname<-a_b_bagplot(community.f=NULL,Td=TdnNA,refindex=1,main="TdnNA")
  }
  # nz<-findnonzerocolumns(x=Td_noise$smat)
  if(fpca){
    calc_pca(x=TdnNA$smat,main="PCA on Original Data")
   # calc_pca(x=TdnNA$ET.x,main="PCA on Equitable Transform")
  }

  }else{
    plotsome(Tdn,images=FALSE,signal=Td$smat,indiv=TRUE,of=TRUE,errb=TRUE ,row_unit=row_unit,col_unit=col_unit,z_unit=z_unit)
    plotsome(Tdn,images=FALSE,signal=Td$smat,transpose=TRUE,indiv=TRUE,of=TRUE,errb=TRUE,row_unit=row_unit,col_unit=col_unit,z_unit=z_unit )
    plotsome(Tdn,signal=Td$smat,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE ,stderror=TRUE,row_unit=row_unit,col_unit=col_unit,z_unit=z_unit)#plots individual columns of Equitable Transform with std error of mean at each point
    plotsome(Tdn,signal=Td$smat,transpose=TRUE,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE ,stderror=TRUE,row_unit=row_unit,col_unit=col_unit,z_unit=z_unit)#plots
    plotsome(Tdn,signal=Td$smat,images=FALSE,versus=TRUE,of=TRUE,lf=TRUE,row_unit=row_unit,col_unit=col_unit,z_unit=z_unit)  #compares
    inc<-round(ncol(Tdn$smat)/10)  # inc<-2
    if(inc<1)inc<-1
    nc<-seq(1,ncol(Tdn$smat),by=inc)

    la<-which(colnames(Tdn$l.s.pslope)=="Row_Ave")
    if(length(la)!=0){pm<-colMeans(Tdn$l.s.pslope[,-la],na.rm=TRUE)
    }  else pm<-colMeans(Tdn$l.s.pslope,na.rm=TRUE)
    refer<-which(pm==min(pm))
    cat("\nbest reference individual is ",colnames(Tdn$l.s.pslope)[refer],"\n")
    if(length(refer)>1) refer<-refer[1]
    xvsrefplot(Td=Tdn,cgroup=nc,ref=refer,br=paste0( "Equitable Profiles with Min Probability Profile")) # ,numb=10   Feb15 2018
    main<-paste("minp reference\n",colnames(Tdn$l.s.pslope)[refer])
    plot_hist(Tdn,refer=refer,main=main)

    main<-paste("\nEntire Matrix")
    plot_hist(Tdn,main=main)
    # nz<-findnonzerocolumns(x=Td_noise$smat)

     findinfo(Tdave=Tdn,printmax=FALSE,numb=1)   #if too many missing then uit fails
      bestintersectname<-a_b_bagplot(community.f=NULL,Td=Tdn,refindex=1,main="Tdn")

    if(mean(Tdn$l.s.r2,na.rm=TRUE)<0.97){
      calc_pca(x=Tdn$smat,main="PCA on Original Data")
      calc_pca(x=Tdn$ET.x,main="PCA on Equitable Transform")
    }

     }

  if(!is.null(TdnNA$ET.x))imagenan(TdnNA$ET.x,main=paste0("ET from Signal+Noise with NAs : rmult= ",
                                                          rmult," cmult= ",cmult,"\nfac=",fac, " fraction of NA=",NAfrac),row_unit=row_unit,col_unit=col_unit,zlim=z_unit)
  if(!is.null(TdnNA$smat))imagenan(TdnNA$smat,main=paste0("Signal+Noise with NAs : rmult= ",
                                                          rmult," cmult= ",cmult,"\nfac=",fac, " fraction of NA=",NAfrac),row_unit=row_unit,col_unit=col_unit,zlim=z_unit)
  if(!is.null(TdNA$smat))imagenan(TdNA$ET.x,main=paste0("ET from Signal with NAs: rmult= ",
                                                        rmult," cmult= ",cmult," fraction of NA=",NAfrac),row_unit=row_unit,col_unit=col_unit,zlim=z_unit)
  if(!is.null(TdNA$smat))imagenan(TdNA$smat,main=paste0("Signal with NAs: rmult= ",
                                                        rmult," cmult= ",cmult," fraction of NA=",NAfrac),row_unit=row_unit,col_unit=col_unit,zlim=z_unit)
  imagenan(Tdn$ET.x,main=paste0("ET from Signal+Noise: rmult= ",rmult," cmult= ",cmult,"\nfac=",fac),row_unit=row_unit,col_unit=col_unit,zlim=z_unit)
  imagenan(Tdn$smat,main=paste0("Signal+Noise: rmult= ",rmult," cmult= ",cmult,"\nfac=",fac),row_unit=row_unit,col_unit=col_unit,zlim=z_unit)
  if(!is.null(Td$smat))imagenan(Td$smat,main=paste0("Signal : rmult= ",rmult," cmult= ",cmult),row_unit=row_unit,col_unit=col_unit,zlim=z_unit)






}   #results from TnormNA plotted abnd summarized

eg0<-function(rmult=1,cmult=1,n=NULL){
  if(is.null(n))n<-2
  #example 0
  cend<-10           #"space" multiplicative factor that increases number of  columns from standard number: rend
  rend<-360             #"time" multiplicative factor that increases number of rows  from standard number: cend
  rnum0<-15; cnum0<-10          # 10 times 15 space

  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  f<- rep(1,length(c))   #replicate 1 r times
  g<-sin(2*pi*(r+30)/900)
  #u<-c/10 +1
  u=abs(c-cend/2)^n/(cend/2)^n+1
  Tx<- make_data(f,g,u,cend,rend,rnum0,cnum0,rmult,cmult)
  return(Tx)
} # "average" f(x)=1, u=abs(c-5)^n/cend^n+1 ,g=long period sine wave 30 rows displaced

eg00<-function(rmult=1,cmult=1,freq=1){
  #example 0
  cend<-10           #"space" multiplicative factor that increases number of  columns from standard number: rend
  rend<-360             #"time" multiplicative factor that increases number of rows  from standard number: cend
  rnum0<-15; cnum0<-10          # 10 times 15 space

  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  f<- rep(1,length(c))   #replicate 1 r times
  g<-cos(2*pi*freq*(r)/360)
  u<-c*0
  Tx<- make_data(f,g,u,cend,rend,rnum0,cnum0,rmult,cmult)
  return(Tx)
} # "average" f(x)=1, u=0 ,g=long period cosine wave

eg01<-function(rmult=1,cmult=1,freq=1){
  #example 0
  cend<-360           #"space" multiplicative factor that increases number of  columns from standard number: rend
  rend<-360             #"time" multiplicative factor that increases number of rows  from standard number: cend
  rnum0<-15; cnum0<-10          # 10 times 15 space

  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  f<- 1+cos(2*pi*1/2*(c)/360)  #
  g<-cos(2*pi*freq*(r)/360)
  u<-c*0
  Tx<- make_data(f,g,u,cend,rend,rnum0,cnum0,rmult,cmult)
  return(Tx)
} # "

eg02<-function(rmult=1,cmult=1,freq=1){
  #example 0
  cend<-360           #"space" multiplicative factor that increases number of  columns from standard number: rend
  rend<-360             #"time" multiplicative factor that increases number of rows  from standard number: cend
  rnum0<-15; cnum0<-10          # 10 times 15 space

  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  f<- 1+cos(2*pi*1/2*(c)/360)  #
  g<-cos(2*pi*freq*(r)/360)+0.5
  u<-c*0
  Tx<- make_data(f,g,u,cend,rend,rnum0,cnum0,rmult,cmult)
  return(Tx)
} # "

eg1<-function(rmult=1,cmult=1){
  #example 1
  cend<-10           #"space" multiplicative factor that increases number of  columns from standard number: rend
  rend<-15             #"time" multiplicative factor that increases number of rows  from standard number: cend
  rnum0<-15; cnum0<-10          # 10 times 15 space

  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  f<- c*1
  g<-r*1
  u<-c*1
  Tx<- make_data(f,g,u,cend,rend,rnum0,cnum0,rmult,cmult)
  return(Tx)
}  #: f=u, g linear functions of r and c  simplifies to f(x)*(g(t)+1)  zero intercept

eg2<-function(rmult=1,cmult=1){
  #example 2  wave in f only
  cend<-360           #"space" multiplicative factor that increases number of  columns from standard number: rend
  rend<-15             #"time" multiplicative factor that increases number of rows  from standard number: cend
  rnum0<-15; cnum0<-10          # 15 times 10 space

  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  f<-sin(2*pi*(c)/180)
  g<-1/5*r
  u<-c/90
  Tx<- make_data(f,g,u,cend,rend,rnum0,cnum0,rmult,cmult)
  return(Tx)
}   #example 2  long wavelength wave in f only : g and u linear in r and c

eg3<-function(rmult=1,cmult=1){
  #example 3    f has no wave but u does
  cend<-360           #"space" multiplicative factor that increases number of  columns from standard number: rend
  rend<-300             #"time" multiplicative factor that increases number of rows  from standard number: cend
  rnum0<-15; cnum0<-10          # 45 times 10 space



  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  f<-(c/90)^(1/2)
  g<-sin(2*pi*(r+30)/300)+sin(2*pi*(r+30)/180)
  u<-0.5*c/360+10*sin(2*pi*c/720)

  Tx<- make_data(f,g,u,cend,rend,rnum0,cnum0,rmult,cmult)
  return(Tx)
}#example 3    f varies as sqrt of c ,u sum of linear term +long wavelength wave : g sum of 2 short period waves (same phase)

#' Example 4 of equitable system
#' f(x) product of sqrt of column index and very large scale wave:
#' u(x) linear in column index
#' g(x) sum of two  waves
#'
#'
#' @param rmult  resolution of sampling of t variable  rmult*15 is the number of points sampled
#' @param cmult  resolution of sampling of x variable  cmult*10 is the number of points sampled
#'
#' @return matrix of equitable data
#'
#' @examples
#' d<-eg4(1,1)       # 15x10 data set
#' d<-eg4(1,10)      # 15x100 data set
#' d<-eg4(10,1)      # 150x10 data set
#' d<-eg4(15,15)    # 150x100 data set
#'
#' @export
eg4<-function(rmult=1,cmult=1){
  cend<-360           #"space" multiplicative factor that increases number of  columns from standard number: rend
  rend<-360             #"time" multiplicative factor that increases number of rows  from standard number: cend
  rnum0<-15; cnum0<-10          # 15 times 10 space         length(r);length(c)
  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  f<-((c/cend)^(1/2)+3)*sin(2*pi*(c+20)/720)
  plot(f,main="Function f(x)")
  g<-sin(2*pi*(r+30)/270)+sin(2*pi*(r+30)/180)
  plot(g,main="Function g(t)")
  u<-3*(c/cend+1)
  plot(u,main="Function u(x)")
  F<-matrix(rep(f,2),nrow=length(f),ncol=2)
  imagenan(t(F),main="Function f(x)")
  U<-matrix(rep(u,2),nrow=length(u),ncol=2)
  imagenan(t(U),main="Function u(x)")
  G<-matrix(rep(g,2),nrow=length(g),ncol=2)
  imagenan(G,main="Function g(t)")
  Tx<- make_data(f,g,u,cend,rend,rnum0,cnum0,rmult,cmult)
  return(Tx)
} #example 4  f sum of sqrt of c and very large scale wave:  u linear in column index, g sum of two waves in phase (not multiple freq)

#' Example 5 of equitable system
#' f(x) product of sqrt of column index and short wavelength wave:
#' u(x) sum of linear in column index with short wavelength
#' g(x) sum of two  waves
#'
#'
#' @param rmult  resolution of sampling of t variable  rmult*15 is the number of points sampled
#' @param cmult  resolution of sampling of x variable  cmult*10 is the number of points sampled
#'
#' @return matrix of equitable data
#'
#' @examples
#' d<-eg5(1,1)       # 15x10 data set
#' d<-eg5(1,10)      # 15x100 data set
#' d<-eg5(10,1)      # 150x10 data set
#' d<-eg5(15,15)    # 150x100 data set
#'
#' @export
eg5<-function(rmult=1,cmult=1){
  cend<-360           #"space" multiplicative factor that increases number of  columns from standard number: rend
  rend<-360             #"time" multiplicative factor that increases number of rows  from standard number: cend
  rnum0<-15; cnum0<-10          # 15 times 10 space        length(r);length(c)       plot(f+u)     plot(g)



  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  f<-(c)^(1/2)*sin(2*pi*(c+20)/90)
  g<-sin(2*pi*(r+30)/180)+sin(2*pi*(r+30)/90)
  u<-0.1*c+10*sin(2*pi*c/60)
  F<-matrix(rep(f,2),nrow=length(f),ncol=2)
  imagenan(t(F),main="Function f(x)")
  U<-matrix(rep(u,2),nrow=length(u),ncol=2)
  imagenan(t(U),main="Function u(x)")
  G<-matrix(rep(g,2),nrow=length(g),ncol=2)
  imagenan(G,main="Function g(t)")
  Tx<- make_data(f,g,u,cend,rend,rnum0,cnum0,rmult,cmult)
  return(Tx)
} #example 5: f sqrt of c * short wavelength wave,  u sum of linear in c and short period wave: g sum of two waves (in phase)multiples

eg6<-function(rmult=1,cmult=1){
  cend<-360           #"space" multiplicative factor that increases number of  columns from standard number: rend
  rend<-15             #"time" multiplicative factor that increases number of rows  from standard number: cend
  rnum0<-15; cnum0<-10          # 10 times 15 space


  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  f<- 1 *tan(pi*(c/(cend+1)))
  g<-r
  u<-c*0


  Tx<- make_data(f,g,u,cend,rend,rnum0,cnum0,rmult,cmult)
  return(Tx)
} #example 6 f is tan of c, u linear in c;  g linear in r

#' Example 7 of equitable system
#' f(x) Absolute value of linear function in column index
#' u(x) 0
#' g(x) step function in row index
#'
#'
#' @param rmult  resolution of sampling of t variable  rmult*15 is the number of points sampled
#' @param cmult  resolution of sampling of x variable  cmult*10 is the number of points sampled
#'
#' @return matrix of equitable data
#'
#' @examples
#' d<-eg7(1,1)       # 15x10 data set
#' d<-eg7(1,10)      # 15x100 data set
#' d<-eg7(10,1)      # 150x10 data set
#' d<-eg7(15,15)    # 150x100 data set
#'
#' @export
eg7<-function(rmult=1,cmult=1){
  cend<-360           #"space" multiplicative factor that increases number of  columns from standard number: rend
  rend<-360             #"time" multiplicative factor that increases number of rows  from standard number: cend
  rnum0<-15; cnum0<-10          # 10 times 15 space


  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  f<- abs(c-cend/2)+20
  g<-ceiling(r/(rend/5))+1
  u<-0*c
  F<-matrix(rep(f,2),nrow=length(f),ncol=2)
  imagenan(t(F),main="Function f(x)")
  # U<-matrix(rep(u,2),nrow=length(u),ncol=2)
  # imagenan(t(U),main="Function u(x)")
  G<-matrix(rep(g,2),nrow=length(g),ncol=2)
  imagenan(G,main="Function g(t)")
#  cat("\n std f ",sd(f),"   std g ",sd(g),"   std u ",sd(u), "    std(f)std(g) ",sd(f)*sd(g))
#  cat("\n sqrt mean (f^2) ",sqrt(mean(f^2)),"   sqrtmean (g^2) ",sqrt(mean(g^2)),"  sqrtmean (u^2) ",sqrt(mean(u^2)), "    sqrt(mean(f^2))*std(g) ",sqrt(mean(f^2))*sd(g))

  Tx<- make_data(f,g,u,cend,rend,rnum0,cnum0,rmult,cmult)
  return(Tx)
} #example 7 f , g amd u are samopled from normal random distributionns

#' Example 8 of equitable system
#'
#' f(x) , g(t) amd u(x) are samopled from normal random distributions.
#' g is then ordered from  lowest to highest
#' Good for simulating equitable phenology systems
#'
#' f(x) random normal distribution with mean mf and standard deviation sdf
#' u(x) random normal distribution with mean mu and standard deviation sdu
#' g(x) random normal distribution with mean mg and standard deviation sdg
#'
#'
#' @param rmult  resolution of sampling of t variable  rmult*15 is the number of points sampled
#' @param cmult  resolution of sampling of x variable  cmult*10 is the number of points sampled
#' @param mf    10 (default) mean of f(x)
#' @param sdf   0.75 (default) standard deviation of f(x)
#' @param mg    0 (default) mean of g(x)
#' @param sdg   1 (default) standard deviation of g(x)
#' @param mu    10 (default) mean of u(x)
#' @param sdu   0.2 (default) standard deviation of u(x)
#'
#'
#' @return matrix of equitable data
#'
#' @examples
#' d<-eg8(rmult=4,cmult=4)       # 60x40 data set
#'  #transform this data set and then show bagplot intersetions
#' Td<-transformE(d=d)
#' aa<-a_b_bagplot(Td=Td,xlim=c(-0.25,0.25),ylim=c(-5,5))
#'
#' d<-eg8(3,3)       # 45x30 data set
#' d<-eg8(1,10)      # 15x100 data set
#' d<-eg8(10,1)      # 150x10 data set
#' d<-eg8(15,15)    # 150x100 data set
#'
#' @export
eg8<-function(rmult=1,cmult=1,mf=10,mg=0,mu=10,sdf=0.75,sdg=1,sdu=0.2){
  cend<-10           #"space" multiplicative factor that increases number of  columns from standard number: rend
  rend<-15             #"time" multiplicative factor that increases number of rows  from standard number: cend
  rnum0<-15; cnum0<-10          # 15 times 10 space        length(r);length(c)       plot(f+u)     plot(g)



  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  set.seed(10)
  f<-rnorm(length(c),mean=mf,sd=sdf)
  set.seed(1339)
  g<-rnorm(length(r),mean=mg,sd=sdg)
  g<-g[order(-g)]
  set.seed(11130)
  u<-rnorm(length(c),mean=mu,sd=sdu)
  set.seed(NULL)
  F<-matrix(rep(f,2),nrow=length(f),ncol=2)
  imagenan(t(F),main="Function f(x)")
  U<-matrix(rep(u,2),nrow=length(u),ncol=2)
  imagenan(t(U),main="Function u(x)")
  G<-matrix(rep(g,2),nrow=length(g),ncol=2)
  imagenan(G,main="Function g(t)")
  Tx<- make_data(f,g,u,cend,rend,rnum0,cnum0,rmult,cmult,main=paste("mf",mf,"mg",mg,"mu",mu,"sdf",sdf,"sdg",sdg,"sdu",sdu))
  return(Tx)
} #example 8:  random f and u and random g

eg9<-function(rmult=1,cmult=1){
  cend<-10           #"space" multiplicative factor that increases number of  columns from standard number: rend
  rend<-15             #"time" multiplicative factor that increases number of rows  from standard number: cend
  rnum0<-15; cnum0<-10          # 15 times 10 space        length(r);length(c)       plot(f+u)     plot(g)



  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  set.seed(10)
  f<- rep(1,length(c))   #replicate 1 r times
  set.seed(1330)
  g<-rnorm(length(r),mean=1,sd=1)
  set.seed(11130)
  u<-rnorm(length(c),mean=5,sd=3)
  set.seed(NULL)
  Tx<- make_data(f,g,u,cend,rend,rnum0,cnum0,rmult,cmult)
  return(Tx)
} #example 8: f=1 and random u and random g

eg4_no_u<-function(rmult=1,cmult=1){
  cend<-360           #"space" multiplicative factor that increases number of  columns from standard number: rend
  rend<-360             #"time" multiplicative factor that increases number of rows  from standard number: cend
  rnum0<-15; cnum0<-10          # 15 times 10 space         length(r);length(c)
  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  f<-((c/cend)^(1/2)+3)*sin(2*pi*(c+20)/720)
  #f<-rep(1,cend)
  g<-sin(2*pi*(r+30)/270)+sin(2*pi*(r+30)/180)
  #g<-cos(pi*(r)/360)
  u<-0*(c/cend+1)
  Tx<- make_data(f,g,u,cend,rend,rnum0,cnum0,rmult,cmult)
  return(Tx)
} #example 4  f sum of sqrt of c and very large scale wave:  u linear in c, g sum of two waves in phase (not multiple freq)

#' Example "simple" of equitable system
#' f(x) , g(t) amd u(x) are define directly as vectors to reate equitable system
#'
#' @param f vector f(x) default c(2,4,6,8)
#' @param u vector u(x) of same length as default f c(4,3,0,1) when NULL it is set to 0
#' @param g vector g(t) default c(-6,-3,0,1,2)
#'
#'
#' @return matrix of equitable data
#'
#' @examples
#' egsimple()       # u(x)=c(4,3,0,1)
#' egsimple(u=NULL)      # u(x)=0
#' d<-egsimple(f=c(1,2,3),g=c(-1,0,1,2,3),u=c(5,-2,6))
#' d<-egsimple(f=c(1,2,3),g=c(-1,0,1,2,3),u=NULL)
#'
#' @export
egsimple<-function(f=c(2,4,6,8),g=c(-6,-3,0,1,2),u=c(4,3,0,1)){  #u<-1
  #example 0
  cend<-length(f)           #"space" multiplicative factor that increases number of  columns from standard number: rend
  rend<-length(g)             #"time" multiplicative factor that increases number of rows  from standard number: cend
  rnum0<-length(g) ; cnum0<-length(f)           # 10 times 15 space
  rmult<-1;cmult<-1
  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  #f<- 3*c   #replicate 1 r times

  if(is.null(u))u<-rep(0,length(f))
  Tx<- make_data(f,g,u,cend,rend,rnum0,cnum0,rmult,cmult)
  return(Tx)
}

egintersect<-function(rmult=1,cmult=1){
  cend<-360           #"space" multiplicative factor that increases number of  columns from standard number: rend

  rnum0<-15; cnum0<-10          # 15 times 10 space        length(r);length(c)       plot(f+u)     plot(g)
  rend<-rnum0*rmult            #"time" multiplicative factor that increases number of rows  from standard number: cend


  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  f<-(c-180)/360+1
  azero<-(rend/2-rend/20)
  g<-(r-azero)^3/(azero)^3*20

  u<-rep(200,length(c))
  cat("\n intersection set to ", azero," with value of ",f[1]*g[azero]+u[1],"\n")
  F<-matrix(rep(f,2),nrow=length(f),ncol=2)
  imagenan(t(F),main="Function f(x)")
  U<-matrix(rep(u,2),nrow=length(u),ncol=2)
  #imagenan(t(U),main="Function u(x)")
  G<-matrix(rep(g,2),nrow=length(g),ncol=2)
  imagenan(G,main="Function g(t)")
  Tx<- make_data(f,g,u,cend,rend,rnum0,cnum0,rmult,cmult)
  return(Tx)
} #example intersect: f from 1/2 to 2   g cubic with 0 at 127 u fixed at 6

egintersectday<-function(rmult=1,cmult=1,days_between_sample=0,amp=40,nfac=0,g0=0,A=200){
  cend<-360           #"space" multiplicative factor that increases number of  columns from standard number: rend

  rnum0<-15; cnum0<-10          # 15 times 10 space        length(r);length(c)       plot(f+u)     plot(g)
  rend<-rnum0*rmult            #"time" multiplicative factor that increases number of rows  from standard number: cend


  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  f<-(c-180)/360+1
  azero<-(rend/2-rend/20)
  g<-(r-azero)^3/(azero)^3*amp+g0

  u<-rep(A,length(c))
  cat("\n intersection set to ", azero," with value of ",f[1]*g[azero]+u[1]," but amp is ",amp," displaced by g0=",g0," and A=",A,"\n")
  F<-matrix(rep(f,2),nrow=length(f),ncol=2)
  imagenan(t(F),main="Function f(x)")
  U<-matrix(rep(u,2),nrow=length(u),ncol=2)
  #imagenan(t(U),main="Function u(x)")
  G<-matrix(rep(g,2),nrow=length(g),ncol=2)
  imagenan(G,main="Function g(t)")
  Tx<- make_data(f,g,u,cend,rend,rnum0,cnum0,rmult,cmult)
  sdd<-sd(Tx,na.rm=TRUE)
  d_noise<-Tx+rnorm(prod(dim(Tx)),mean=0,sd=nfac*sdd)    #normal distribution with std dev of fac*d's std dev
  Tx<-matrix(d_noise,nrow=nrow(Tx),ncol=ncol(Tx))
  if(days_between_sample!=0){
  Tx<-days_between_sample*ceiling(Tx/days_between_sample)
  }
  rownames(Tx)<-r
  colnames(Tx)<-c
  imagenan(Tx,main="Data (x,t) resolution (days) for sampling=", days_between_sample)
  # for(c in 1:nrow(Tx)){
  #   for(r in 1:ncol(Tx)){
  #    Tx[r,c]<-4*ceiling(Tx[r,c]/4)
  #   }
  # }
  return(Tx)
} #example intersect: f from 1/2 to 2   g cubic with 0 at 127 u fixed at 6
#g0 moves intersection point up (negative) or down (positive) the event list
egintersecttwo<-function(rmult,cmult,days_between_sample=0,amp=40,amp2=20,A=200,A2=210,nfac=0,g0=0,g2=5){
  d<-egintersectday(10,5,amp=amp,g0=g0,A=A);plot(d[,20])
  d1<-egintersectday(10,5,amp=amp2,g0=g2,A=A2);plot(d1[,20]);lines(d[,20])
  colnames(d)<-paste0(colnames(d1),"_a",amp,"_g",g0,"_A",A)
  colnames(d1)<-paste0(colnames(d1),"_a",amp2,"_g",g2,"_A",A2)
  Tx<-cbind(d,d1)
  imagenan(Tx)
  return(Tx)
}
takeaway<-function(e1){
  rm<-rowMeans(e1)
  cm<-colMeans(e1)
  me<-mean(e1)
  for(r in 1:nrow(e1))for(c in 1:ncol(e1))e1[r,c]<-e1[r,c]-rm[r]-cm[c]+me
  imagenan(e1)
  return(e1)
}

egtrav<-function(rmult=1,cmult=1){
  cend<-10           #"space" multiplicative factor that increases number of  columns from standard number: rend
  rend<-10             #"time" multiplicative factor that increases number of rows  from standard number: cend
  rnum0<-10; cnum0<-10          # 15 times 10 space        length(r);length(c)       plot(f+u)     plot(g)



  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  f<-rep(1,length(c))
  g<-sin(2*pi*(r+c)/(rend))
  u<-0*c
  ti<-t(1:(rnum0*rmult)) ; x<-t(1:(cnum0*cmult))
  d<-matrix(NA,nrow=length(ti),ncol=length(x))

  for(a in ti){
    for(b in x){
      #cat(a,b,"\n")
      d[a,b]<-sin(2*pi*(r[a]+c[b])/(rend))
    }
    }

  rownames(d)<-ti; colnames(d)<-x
  imagenan(d,main=paste0("SIGNAL: rmult= ",rmult," cmult= ",cmult))
  Tx<- d
  return(Tx)
} #example 8:  f=1 and u=0 and g=sin(2*pi*(r+c)/(rend))

egstandtrav<-function(rmult=1,cmult=1){
  cend<-10           #"space" multiplicative factor that increases number of  columns from standard number: rend
  rend<-10             #"time" multiplicative factor that increases number of rows  from standard number: cend
  rnum0<-10; cnum0<-10          # 15 times 10 space        length(r);length(c)       plot(f+u)     plot(g)

  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  f<- c*1
  g<-r*1
  u<-c*1
  A<- make_data(f,g,u,cend,rend,rnum0,cnum0,rmult,cmult)

  ti<-t(1:(rnum0*rmult)) ; x<-t(1:(cnum0*cmult))
  d<-matrix(NA,nrow=length(ti),ncol=length(x))

  for(a in ti){
    for(b in x){
      #cat(a,b,"\n")
     # d[a,b]<-30*sin(2*pi*(r[a]+c[b])/(rend))
      d[a,b]<-sin(2*pi*(r[a]+c[b]))

    }
  }
  d<-4*d+A
  rownames(d)<-ti; colnames(d)<-x
  imagenan(d,main=paste0("SIGNAL: rmult= ",rmult," cmult= ",cmult))
  Tx<- d
  return(Tx)
} #example : separable function+travelling wave f=1 and u=0 and g=sin(2*pi*(r+c)/(rend))
egstand<-function(rmult=1,cmult=1){
  cend<-10           #"space" multiplicative factor that increases number of  columns from standard number: rend
  rend<-10             #"time" multiplicative factor that increases number of rows  from standard number: cend
  rnum0<-10; cnum0<-10          # 15 times 10 space        length(r);length(c)       plot(f+u)     plot(g)

  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  f<- c*1
  g<-r*1
  u<-c*1
  A<- make_data(f,g,u,cend,rend,rnum0,cnum0,rmult,cmult)

  ti<-t(1:(rnum0*rmult)) ; x<-t(1:(cnum0*cmult))
  d<-matrix(NA,nrow=length(ti),ncol=length(x))

  for(a in ti){
    for(b in x){
      #cat(a,b,"\n")
      # d[a,b]<-30*sin(2*pi*(r[a]+c[b])/(rend))
      d[a,b]<-sin(2*pi*(r[a]+c[b]))

    }
  }
  d<-A
  rownames(d)<-ti; colnames(d)<-x
  imagenan(d,main=paste0("SIGNAL: rmult= ",rmult," cmult= ",cmult))
  Tx<- d
  return(Tx)
}
egstrav<-function(rmult=1,cmult=1){
  cend<-10           #"space" multiplicative factor that increases number of  columns from standard number: rend
  rend<-10             #"time" multiplicative factor that increases number of rows  from standard number: cend
  rnum0<-10; cnum0<-10          # 15 times 10 space        length(r);length(c)       plot(f+u)     plot(g)

  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  f<- c*1
  g<-r*1
  u<-c*1
  A<- make_data(f,g,u,cend,rend,rnum0,cnum0,rmult,cmult)

  ti<-t(1:(rnum0*rmult)) ; x<-t(1:(cnum0*cmult))
  d<-matrix(NA,nrow=length(ti),ncol=length(x))

  for(a in ti){
    for(b in x){
      #cat(a,b,"\n")
      # d[a,b]<-30*sin(2*pi*(r[a]+c[b])/(rend))
      d[a,b]<-sin(2*pi*(r[a]+c[b]))

    }
  }
  d<-4*d
  rownames(d)<-ti; colnames(d)<-x
  imagenan(d,main=paste0("SIGNAL: rmult= ",rmult," cmult= ",cmult))
  Tx<- d
  return(Tx)
}

egstandstand<-function(rmult=1,cmult=1){
  cend<-10           #"space" multiplicative factor that increases number of  columns from standard number: rend
  rend<-10             #"time" multiplicative factor that increases number of rows  from standard number: cend
  rnum0<-10; cnum0<-10          # 15 times 10 space        length(r);length(c)       plot(f+u)     plot(g)

  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  f<- c*1
  g<-r*1
  u<-c*1
  A<- make_data(f,g,u,cend,rend,rnum0,cnum0,rmult,cmult)

  ti<-t(1:(rnum0*rmult)) ; x<-t(1:(cnum0*cmult))
  d<-matrix(NA,nrow=length(ti),ncol=length(x))

  for(a in ti){
    for(b in x){

      d[a,b]<-cos(2*pi*(r[a]))*sin(2*pi*(c[b]))

    }
  }
  d<-4*d+A
  rownames(d)<-ti; colnames(d)<-x
  imagenan(d,main=paste0("SIGNAL: rmult= ",rmult," cmult= ",cmult))
  Tx<- d
  return(Tx)
} #example : separable function+
egstandfirst<-function(rmult=1,cmult=1){
  cend<-10           #"space" multiplicative factor that increases number of  columns from standard number: rend
  rend<-10             #"time" multiplicative factor that increases number of rows  from standard number: cend
  rnum0<-10; cnum0<-10          # 15 times 10 space        length(r);length(c)       plot(f+u)     plot(g)

  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  f<- c*1
  g<-r*1
  u<-c*1
  A<- make_data(f,g,u,cend,rend,rnum0,cnum0,rmult,cmult)

  ti<-t(1:(rnum0*rmult)) ; x<-t(1:(cnum0*cmult))
  d<-matrix(NA,nrow=length(ti),ncol=length(x))

  for(a in ti){
    for(b in x){

      d[a,b]<-cos(2*pi*(r[a]))*sin(2*pi*(c[b]))

    }
  }
  d<-A
  rownames(d)<-ti; colnames(d)<-x
  imagenan(d,main=paste0("SIGNAL: rmult= ",rmult," cmult= ",cmult))
  Tx<- d
  return(Tx)
} #exa
egstandsecond<-function(rmult=1,cmult=1){
  cend<-10           #"space" multiplicative factor that increases number of  columns from standard number: rend
  rend<-10             #"time" multiplicative factor that increases number of rows  from standard number: cend
  rnum0<-10; cnum0<-10          # 15 times 10 space        length(r);length(c)       plot(f+u)     plot(g)

  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  f<- c*1
  g<-r*1
  u<-c*1
  A<- make_data(f,g,u,cend,rend,rnum0,cnum0,rmult,cmult)

  ti<-t(1:(rnum0*rmult)) ; x<-t(1:(cnum0*cmult))
  d<-matrix(NA,nrow=length(ti),ncol=length(x))

  for(a in ti){
    for(b in x){

      d[a,b]<-cos(2*pi*(r[a]))*sin(2*pi*(c[b]))

    }
  }
  d<-4*d
  rownames(d)<-ti; colnames(d)<-x
  imagenan(d,main=paste0("SIGNAL: rmult= ",rmult," cmult= ",cmult))
  Tx<- d
  return(Tx)
} #exa

egsep<-function(rmult=1,cmult=1,Acx=1,mu=1,omega=1){
  cend<-360           #"space" multiplicative factor that increases number of  columns from standard number: rend
  rend<-360             #"time" multiplicative factor that increases number of rows  from standard number: cend
  rnum0<-15; cnum0<-10          # 15 times 10 space         length(r);length(c)
  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  f<-Acx*sin(mu*pi*(c)/cend)
  g<-cos(omega*pi*(r)/rend)
  u<-0*(c/cend+1)
  Tx<- make_data(f,g,u,cend,rend,rnum0,cnum0,rmult,cmult)
  return(Tx)
} #example

egsep1<-function(rmult=1,cmult=1,Asx=1,Acx=1,Ast=1,Act=1,mu=1,omega=1){
  cend<-360           #"space" multiplicative factor that increases number of  columns from standard number: rend
  rend<-360             #"time" multiplicative factor that increases number of rows  from standard number: cend
  rnum0<-15; cnum0<-10          # 15 times 10 space         length(r);length(c)
  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  f<-Acx*cos(mu*pi*(c)/cend)+Asx*sin(mu*pi*(c)/cend)
  g<-Act*cos(omega*pi*(r)/rend)+Ast*sin(omega*pi*(r)/rend)
  u<-0*(c/cend+1)
  Tx<- make_data(f,g,u,cend,rend,rnum0,cnum0,rmult,cmult)
  return(Tx)
} #example

egseph1<-function(rmult=1,cmult=1,Asx=1,Acx=1,Ast=1,Act=1,mu=1,omega=1){
  cend<-360           #"space" multiplicative factor that increases number of  columns from standard number: rend
  rend<-360             #"time" multiplicative factor that increases number of rows  from standard number: cend
  rnum0<-15; cnum0<-10          # 15 times 10 space         length(r);length(c)
  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  f<-Acx*cosh(mu*pi*(c)/cend)+Asx*sinh(mu*pi*(c)/cend)
  g<-Act*cosh(omega*pi*(r)/rend)+Ast*sinh(omega*pi*(r)/rend)
  u<-0*(c/cend+1)
  Tx<- make_data(f,g,u,cend,rend,rnum0,cnum0,rmult,cmult)
  Tx<-1/max(Tx)*Tx
  Tx<-takeaway(Tx)
  return(Tx)
} #example

egtrav2<-function(rmult=1,cmult=1){
  cend<-10           #"space" multiplicative factor that increases number of  columns from standard number: rend
  rend<-10             #"time" multiplicative factor that increases number of rows  from standard number: cend
  rnum0<-10; cnum0<-10          # 15 times 10 space        length(r);length(c)       plot(f+u)     plot(g)



  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  f<-rep(1,length(c))
  g<-sin(2*pi*(r+2*c)/(rend))
  u<-0*c
  ti<-t(1:(rnum0*rmult)) ; x<-t(1:(cnum0*cmult))
  d<-matrix(NA,nrow=length(ti),ncol=length(x))

  for(a in ti){
    for(b in x){
      #cat(a,b,"\n")
      d[a,b]<-sin(2*pi*(r[a]+2*c[b])/(rend))
    }
  }

  rownames(d)<-ti; colnames(d)<-x
  imagenan(d,main=paste0("SIGNAL: rmult= ",rmult," cmult= ",cmult))
  Tx<- d
  return(Tx)
} #example 8:  f=1 and u=0 and g=sin(2*pi*(r+c)/(rend))

eg4rand<-function(rmult=1,cmult=1){
  cend<-360           #"space" multiplicative factor that increases number of  columns from standard number: rend
  rend<-360             #"time" multiplicative factor that increases number of rows  from standard number: cend
  rnum0<-15; cnum0<-10          # 15 times 10 space         length(r);length(c)
  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  f<-((c/cend)^(1/2)+3)*sin(2*pi*(c+20)/720)
  g<-sin(2*pi*(r+30)/270)+sin(2*pi*(r+30)/180)
  u<-3*(c/cend+1)
  Tx<- make_data(f,g,u,cend,rend,rnum0,cnum0,rmult,cmult)
  newcol<-sample(1:ncol(Tx), ncol(Tx), replace=F)
  newrow<-sample(1:nrow(Tx), nrow(Tx), replace=F)
  Tx[,newcol]<-Tx[,1:ncol(Tx)]
  Tx[newrow,]<-Tx[1:nrow(Tx),]
  return(Tx)
} #example 4  f sum of sqrt of c and very large scale wave:  u linear in c, g sum of two waves then randomize row/columns

egtrav10<-function(rmult=1,cmult=1){
  cend<-10           #"space" multiplicative factor that increases number of  columns from standard number: rend
  rend<-10             #"time" multiplicative factor that increases number of rows  from standard number: cend
  rnum0<-10; cnum0<-10          # 15 times 10 space        length(r);length(c)       plot(f+u)     plot(g)



  r<- seq(rend/rnum0*(1/rmult),rend, by=rend/rnum0*(1/rmult))
  c<-seq(cend/cnum0*(1/cmult),cend, by=cend/cnum0*(1/cmult))
  f<-rep(1,length(c))
  g<-sin(2*pi*(r+2*c)/(rend))
  u<-0*c
  ti<-t(1:(rnum0*rmult)) ; x<-t(1:(cnum0*cmult))
  d<-matrix(NA,nrow=length(ti),ncol=length(x))

  for(a in ti){
    for(b in x){
      #cat(a,b,"\n")
      d[a,b]<-sin(2*pi*(r[a]+10*c[b])/(rend))
    }
  }

  rownames(d)<-ti; colnames(d)<-x
  imagenan(d,main=paste0("SIGNAL: rmult= ",rmult," cmult= ",cmult))
  Tx<- d
  return(Tx)
} #ex

changenoisenormalcol<-function(rmult=1,cmult=1, start, end, inc,
                             actualFunction){
  sdx<-sapply(seq(start,end, by=inc), FUN=function(rmult,cmult,fac,eg,diagonal){
    cat("\n factor for noise is",fac,"\n")
    Td<-Tnormcol(rmult,cmult,fac,noise=FALSE, FUN=eg,diagonal=diagonal)
    Td_noise<-Tnormcol(rmult,cmult,fac, FUN=eg,diagonal=diagonal)
    runstatsNS(Td,Td_noise)
    sdE<-sd(Td_noise$ET.x-Td$ET.x, na.rm = TRUE)
    sdls<-sd(Td_noise$l.s.x-Td$ET.x, na.rm = TRUE)
    sdEave<-sd(Td_noise$Ave.ET.x-Td$ET.x, na.rm = TRUE)
    sdnoise<-sd(Td_noise$smat-Td$ET.x, na.rm = TRUE)
    fsqrt<-sqrt(1/nrow(Td$smat)+1/ncol(Td$smat))
    sdsqrt<-fsqrt*sdnoise
    sigorig<-sd(Td_noise$smat, na.rm = TRUE)

    lm.ls_vs_sig <- lm(c(Td_noise$l.s.x) ~ c(Td$smat), na.action=na.exclude)
    b<-coef(lm.ls_vs_sig)[1]; a<-coef(lm.ls_vs_sig)[2]
    cat("\nLeast squared vs signal linear fit \n")
    #print(summary(lm.ls_vs_sig))
    #a<-(0.887)^2  ; b<- 0.54 ;storig<-2.42; sig0<-0.67;
    ms<-mean(Td$smat, na.rm=TRUE)
    #sdcalc<-sqrt((a-1)^2*sigorig^2+(a^2)*(sdsqrt)^2+b^2  +(a-1)*b*ms)   # pre feb 2019 version
    sdcalc<-sqrt((a-1)^2*(sigorig^2+ms^2) +2*(a-1)*b*ms+(a^2)*(sdsqrt)^2+b^2 )
    noisedata<-Td_noise$smat-Td$ET.x
    cnoise<-rowMeans(noisedata, na.rm = TRUE)
    rnoise<-colMeans(noisedata, na.rm = TRUE)
    noiseavedata<-noisedata;noiseavedata[]<-NA
    for(c in 1:ncol(noisedata))for(r in 1:nrow(noisedata))noiseavedata[r,c]<-cnoise[c]*rnoise[r]
    sdnoiseav<-sd(noiseavedata, na.rm = TRUE)
    sdcalc1<-sqrt((a-1)^2*(sigorig^2+ms^2) +2*(a-1)*b*ms+(a^2)*(sdnoiseav)^2+b^2 )

    sdall<-list(fac=fac,rmult=rmult,cmult=cmult,nr=nrow(Td$smat),nc=ncol(Td$smat),sdnoise=sdnoise,
                sdE= sdE,sdls=sdls,sdEave=sdEave,fsqrt= fsqrt,sdsqrt=sdsqrt,sdcalc=sdcalc,sdcalc1=sdcalc1)
    return(sdall)
  },rmult=rmult,cmult=cmult, eg=actualFunction,simplify = TRUE,diagonal=TRUE )
  sdx<-matrix(as.numeric(sdx), nrow=nrow(sdx),ncol=ncol(sdx),
              dimnames=list(rownames(sdx),colnames(sdx)))
  plot(sdx["fac",], sdx["sdnoise",],ylim=c(0,max(sdx["sdnoise",])),
       main=paste0(" squares=Equitable,ls=Stars,noise=circles\nNorm col: line= lowest std dev \n",
                   "cmult= ", cmult," rmult=",rmult))
  lines(sdx["fac",], sdx["sdsqrt",])
  lines(sdx["fac",], sdx["sdls",],type="p",pch=11)
  #lines(sdx["fac",], sdx["fsqrt",])
  lines(sdx["fac",], sdx["sdE",],type="p",pch=15)
  lines(sdx["fac",], sdx["sdcalc",])
  lines(sdx["fac",], sdx["sdcalc1",])
  lines(sdx["fac",], sdx["sdEave",],type="p",pch="O")
  print(sdx)
  return(sdx)
} #run normcol noise levels(c*std dev) from c=start,end by inc for actualfunction=eg1,eg2,..etc and plot
changenoisenormal<-function(rmult=1,cmult=1,start, end, inc,
                            actualFunction,Ave=TRUE,diagonal=TRUE,pf=FALSE,ipf=FALSE,C=1.028){
  sdx<-sapply(seq(start,end, by=inc), FUN=function(rmult,cmult,fac,eg,Ave,diagonal,ipf,pf,C){
    cat("\n factor for noise is",fac,"\n")
    Td<-Tnorm(rmult,cmult,fac,noise=FALSE, FUN=eg,Ave=Ave,diagonal=diagonal)
    Td_noise<-Tnorm(rmult,cmult,fac, FUN=eg,Ave=Ave,diagonal=diagonal)
    runstatsNS(Td,Td_noise)
    sdE<-sd(Td_noise$ET.x-Td$ET.x, na.rm = TRUE)
    sdls<-sd(Td_noise$l.s.x-Td$ET.x, na.rm = TRUE)
    sdEave<-sd(Td_noise$Ave.ET.x-Td$ET.x, na.rm = TRUE)
    sdnoise<-sd(Td_noise$smat-Td$ET.x, na.rm = TRUE)
    fsqrt<-sqrt(1/nrow(Td$smat)+1/ncol(Td$smat))
    sdsqrt<-fsqrt*sdnoise
    sigorig<-sd(Td_noise$smat, na.rm = TRUE)

    lm.ls_vs_sig <- lm(c(Td_noise$l.s.x) ~ c(Td$smat), na.action=na.exclude)
    b<-coef(lm.ls_vs_sig)[1]; a<-coef(lm.ls_vs_sig)[2]
    cat("\nLeast squared vs signal linear fit \n")
    #print(summary(lm.ls_vs_sig))
    #a<-(0.887)^2  ; b<- 0.54 ;storig<-2.42; sig0<-0.67;
    ms<-mean(Td$smat, na.rm=TRUE)
    #sdcalc<-sqrt((a-1)^2*sigorig^2+(a^2)*(sdsqrt)^2+b^2  +(a-1)*b*ms)  #this is pre feb 2019 version
    #sigorig is signal std dev
    sdcalc<-sqrt((a-1)^2*(sigorig^2+ms^2) +2*(a-1)*b*ms+(a^2)*(sdsqrt)^2+b^2 )   #feb 16 2019 version with signal sd is sigorig
    noisedata<-Td_noise$smat-Td$ET.x
    cnoise<-rowMeans(noisedata, na.rm = TRUE)
    rnoise<-colMeans(noisedata, na.rm = TRUE)
    noiseavedata<-noisedata;noiseavedata[]<-NA
    for(c in 1:ncol(noisedata))for(r in 1:nrow(noisedata))noiseavedata[r,c]<-cnoise[c]*rnoise[r]
    sdnoiseav<-sd(noiseavedata, na.rm = TRUE)
    sdcalc1<-sqrt((a-1)^2*(sigorig^2+ms^2) +2*(a-1)*b*ms+(a^2)*(sdnoiseav)^2+b^2 )
    stdvalues<-stats_residuals(Td_noise,Td=Td,ipf=ipf,pf=pf,C=C)

    sdall<-list(fac=fac,rmult=rmult,cmult=cmult,nr=nrow(Td$smat),nc=ncol(Td$smat),sdnoise=sdnoise,
                theory_sdN_Ave_plus  =stdvalues$theory_sdN_Ave_plus, sdnoise_approx = stdvalues$sdnoise_approx ,theory_sdN_Ave=stdvalues$theory_sdN_Ave,
                sdE= sdE,sdls=sdls,sdEave=sdEave,fsqrt= fsqrt,sdsqrt=sdsqrt,sdcalc=sdcalc,sdcalc1=sdcalc1)
    print(sdall)
    return(sdall)
  },rmult=rmult,cmult=cmult, eg=actualFunction,simplify = TRUE,Ave=Ave ,diagonal=diagonal,ipf=ipf,pf=pf,C=C)
  sdx<-matrix(as.numeric(sdx), nrow=nrow(sdx),ncol=ncol(sdx),
              dimnames=list(rownames(sdx),colnames(sdx)))
  plot(sdx["fac",], sdx["sdnoise",],ylim=c(0,max(sdx["sdnoise",])),ylab="Standard Deviation",xlab="Fraction of Original Signal",
       main=paste0("Standard deviation vs Fraction of Original Signal noise is \n",
                   "cmult= ", cmult," rmult",rmult," C=",C))
  pch<-t((c(1,15,11,NA,NA,NA,NA,NA)))
  # legend<-c('Original','Equitable','Least Squared','Minimum',"Least Squares (Bias)")
  # lty<-c(NA,NA,NA,1,2)
  # lwd<-c(NA,NA,NA,4,2)
  legend<-c('Noise:I-S','T-S','L.S.-S','Row/Col Ave Noise',"Predicted:L.S.(Bias)","Predicted:L.S.(Bias1)", "Predicted Noise","Predicted:T-S using C" )
  lty<-c(NA,NA,NA,1,2,3,   4,5)
  lwd<-c(NA,NA,NA,4,2,2,   3,2)

  legend('topleft',inset=.00,legend=legend,
         lwd=lwd,lty=lty,pch = pch,bg='white',ncol=c(2),cex=0.75)

  #lines(sdx["fac",], sdx["sdsqrt",],lty=1,lwd=4)
  lines(sdx["fac",], sdx["sdE",],type="p",pch=15) #T-S
  lines(sdx["fac",], sdx["sdls",],type="p",pch=11) #T-ls


  lines(sdx["fac",], sdx["theory_sdN_Ave",],lty=1,lwd=4) #theory_sdN_Ave=stdvalues$theory_sdN_Ave sigN*sqrt
  lines(sdx["fac",], sdx["sdcalc",],lty=2,lwd=2)         # ls bias preidct using sigN*sqrt
  lines(sdx["fac",], sdx["sdcalc1",],lty=3,lwd=2)        # ls bias preidct using true noise aves
  lines(sdx["fac",], sdx["sdnoise_approx",],lty=4,lwd=3)  #Noise apporx using ?
  lines(sdx["fac",], sdx["theory_sdN_Ave_plus",],lty=5,lwd=2)  # predicted T-S using extra factor in 1/x+1/T
  #lines(sdx["fac",], sdx["sdEave",],type="p",pch=24)

  return(sdx)
}#run normal noise levels(c*std dev) from c=start,end by inc for actualfunction=eg1,eg2,..etc and plot

changeNAnormal<-function(rmult, cmult,start, end, inc,fac,
                            actualFunction,Ave=TRUE,diagonal=TRUE,pf=FALSE,ipf=FALSE,C=1.028){
  sdx<-sapply(seq(start,end, by=inc), FUN=function(rmult,cmult,fac,eg,Ave,diagonal,NAfrac,ipf,pf,C){
    cat("\n proportion of NA values is",NAfrac,"\n")
    T4<-TnormNA(rmult,cmult,fac=fac, FUN=eg,Ave=Ave,diagonal=diagonal,NAfrac=NAfrac,imageplot=TRUE)
    Td<-T4$Td
    Td_noise<-T4$TdnNA
    imagenan(Td_noise$ET.x,main=paste0("Equitable Transform from noisy data\n with ",NAfrac," missing data"))
    runstatsNS(Td,Td_noise)
    sdE<-sd(Td_noise$ET.x-Td$ET.x, na.rm = TRUE)
    sdls<-sd(Td_noise$l.s.x-Td$ET.x, na.rm = TRUE)
    sdEave<-sd(Td_noise$Ave.ET.x-Td$ET.x, na.rm = TRUE)
    sdnoise<-sd(Td_noise$smat-Td$ET.x, na.rm = TRUE)
    fsqrt<-sqrt(1/nrow(Td$smat)+1/ncol(Td$smat))

    sumNA<-sum(length(which(is.na(Td_noise$smat))))

    Nt<-prod(dim(Td_noise$smat))
    fr<-sumNA/Nt
    # cat("\ntotal of NA transformed data is ",sumNA," with total ",Nt," Fraction of data set that is NA is ",fr )
    fac<-sqrt(1-fr)
    newM<-nrow(Td_noise$smat)*fac
    newN<-ncol(Td_noise$smat)*fac
    sdfactornew<- sqrt(1/(newN-1)+1/(newM-1))
    sdfactor<- sqrt(1/(nrow(Td_noise$smat)-1)+1/(ncol(Td_noise$smat)-1))
    # cat("\n  col= ",ncol(Td_noise$smat),"  row= ",nrow(T_noise$smat), " initial scale factor= ",sdfactor)
    # cat("\neffective  col= ",newN,"effective  row= ",newM, " scale factor= ",sdfactornew, "final = ",sdnoise*sdfactornew)
    sdsqrtnew<-sdnoise*sdfactornew
    sdsqrt<-sdfactor*sdnoise

    sigorig<-sd(Td_noise$smat, na.rm = TRUE)

    lm.ls_vs_sig <- lm(c(Td_noise$l.s.x) ~ c(Td$smat), na.action=na.exclude)
    b<-coef(lm.ls_vs_sig)[1]; a<-coef(lm.ls_vs_sig)[2]
    cat("\nLeast squared vs signal linear fit \n")
    #print(summary(lm.ls_vs_sig))
    #a<-(0.887)^2  ; b<- 0.54 ;storig<-2.42; sig0<-0.67;
    ms<-mean(Td$smat, na.rm=TRUE)
    #sdcalc<-sqrt((a-1)^2*sigorig^2+(a^2)*(sdsqrt)^2+b^2  +(a-1)*b*ms)
    #sdcalc<-sqrt((a-1)^2*sigorig^2+(a^2)*(sdsqrt)^2+b^2  +(a-1)*b*ms)   # pre feb 2019 version
    sdcalc<-sqrt((a-1)^2*(sigorig^2+ms^2) +2*(a-1)*b*ms+(a^2)*(sdsqrt)^2+b^2 )
    noisedata<-Td_noise$smat-Td$ET.x
    cnoise<-rowMeans(noisedata, na.rm = TRUE)
    rnoise<-colMeans(noisedata, na.rm = TRUE)
    noiseavedata<-noisedata;noiseavedata[]<-NA
    for(c in 1:ncol(noisedata))for(r in 1:nrow(noisedata))noiseavedata[r,c]<-cnoise[c]*rnoise[r]
    sdnoiseav<-sd(noiseavedata, na.rm = TRUE)
    sdcalc1<-sqrt((a-1)^2*(sigorig^2+ms^2) +2*(a-1)*b*ms+(a^2)*(sdnoiseav)^2+b^2 ) #could use sdE as apporx?

    stdvalues<-stats_residuals(Td_noise,Td=Td,ipf=ipf,pf=pf,C=C)

    sdall<-list(NAfrac=NAfrac,fac=fac,rmult=rmult,cmult=cmult,nr=nrow(Td$smat),nc=ncol(Td$smat),sdnoise=sdnoise,
                theory_sdN_Ave_plus  =stdvalues$theory_sdN_Ave_plus, sdnoise_approx = stdvalues$sdnoise_approx ,theory_sdN_Ave=stdvalues$theory_sdN_Ave,
                sdE= sdE,sdls=sdls,sdEave=sdEave,fsqrt= fsqrt,sdsqrt=sdsqrt,sdsqrtnew=sdsqrtnew,sdcalc=sdcalc,sdcalc1=sdcalc1)
    print(sdall)
    return(sdall)
  },rmult=rmult,cmult=cmult,fac=fac, eg=actualFunction,simplify = TRUE ,Ave=Ave,diagonal=diagonal,ipf=ipf,pf=pf,C=C)
  sdx<-matrix(as.numeric(sdx), nrow=nrow(sdx),ncol=ncol(sdx),
              dimnames=list(rownames(sdx),colnames(sdx)))

  # plot(sdx["NAfrac",], sdx["sdnoise",],ylim=c(0,max(sdx["sdnoise",],sdx["sdE",])),
  #      main=paste0("squares=Equitable,ls=Stars,noise=circles\nNormal: line= lowest std dev \n",
  #                  "cmult= ", cmult," rmult=",rmult," fac=",fac," C=",C ))
  # lines(sdx["NAfrac",], sdx["sdsqrt",])
  # #lines(sdx["NAfrac",], sdx["sdsqrtnew",])
  # lines(sdx["NAfrac",], sdx["sdls",],type="p",pch=11)
  # #lines(sdx["NAfrac",], sdx["fsqrt",])
  # lines(sdx["NAfrac",], sdx["sdE",],type="p",pch=15)
  # #lines(sdx["NAfrac",], sdx["sdcalc",])
  # #lines(sdx["NAfrac",], sdx["sdEave",],type="p",pch="O")
  #
  plot(sdx["NAfrac",], sdx["sdnoise",],ylim=c(0,max(sdx["sdnoise",])),ylab="Standard Deviation",xlab="Missing Fraction of Original Signal",
       main=paste0("Standard Deviation vs Missing Fraction of Original Signal \n",
                   "cmult= ", cmult," rmult",rmult),pch=16,cex=1.5)
  # pch<-t((c(1,15,11,NA,NA)))
  # legend<-c('Original','Equitable','Least Squared','Minimum',"Least Squares (Bias)")
  # lty<-c(NA,NA,NA,1,2)
  # lwd<-c(NA,NA,NA,4,2)
  # legend('topleft',inset=.02,legend=legend,
  #        lwd=lwd,lty=lty,pch = pch,bg='white',ncol=c(2),cex=0.75)

  pch<-t((c(16,15,17,NA,NA,NA,NA,NA)))
  legend<-c('Noise:I-S','T-S','L.S.-S','Row/Col Ave Noise',"Predicted:L.S.(Bias)","Predicted:L.S.(Bias1)", "Predicted Noise","Predicted:T-S" )
  lty<-c(NA,NA,NA,1,3,3,   4,5)
  lwd<-c(NA,NA,NA,4,2,3,   3,2)
   legend('bottomleft',inset=.00,legend=legend,
         lwd=lwd,lty=lty,pch = pch,bg='white',ncol=c(2),cex=0.75)
  #lines(sdx["NAfrac",], sdx["sdsqrt",],lty=1,lwd=4)
  lines(sdx["NAfrac",], sdx["theory_sdN_Ave",],lty=1,lwd=4)
  lines(sdx["NAfrac",], sdx["sdls",],type="p",pch=17,cex=1.5)
  #lines(sdx["NAfrac",], sdx["fsqrt",],lty=1,lwd=4)
  lines(sdx["NAfrac",], sdx["sdE",],type="p",pch=15,cex=1.5)
  lines(sdx["NAfrac",], sdx["sdcalc",],lty=3,lwd=2)
  lines(sdx["NAfrac",], sdx["sdcalc1",],lty=3,lwd=3)
  lines(sdx["NAfrac",], sdx["sdnoise_approx",],lty=4,lwd=4)
  lines(sdx["NAfrac",], sdx["theory_sdN_Ave_plus",],lty=5,lwd=2)
  #lines(sdx["NAfrac",], sdx["sdEave",],type="p",pch=24)

  # lines(sdx["NAfrac",], sdx["sdsqrt",],lty=1,lwd=4)
  # lines(sdx["NAfrac",], sdx["sdls",],type="p",pch=11)
  # #lines(sdx["NAfrac",], sdx["fsqrt",],lty=1,lwd=4)
  # lines(sdx["NAfrac",], sdx["sdE",],type="p",pch=15)
  # lines(sdx["NAfrac",], sdx["sdcalc",],lty=2,lwd=2)
  # #lines(sdx["NAfrac",], sdx["sdEave",],type="p",pch=24)
  print(sdx)
  return(sdx)
}#run normal noise levels(c*std dev) from c=start,end by inc for actualfunction=eg1,eg2,..etc and plot

runsd<-function(start, end, cmult,
                actualFunction){
  sdx<-sapply(start:end, FUN=function(rmult,cmult,eg,diagonal){

    Td<-T(rmult,cmult,noise=FALSE, FUN=eg,diagonal=diagonal)
    Td_noise<-T(rmult,cmult, FUN=eg,diagonal=diagonal)
    runstatsNS(Td,Td_noise)
    sdE<-sd(Td_noise$ET.x-Td$ET.x, na.rm = TRUE)
    sdls<-sd(Td_noise$l.s.x-Td$ET.x, na.rm = TRUE)
    sdEave<-sd(Td_noise$Ave.ET.x-Td$ET.x, na.rm = TRUE)
    sdnoise<-sd(Td_noise$smat-Td$ET.x, na.rm = TRUE)
    fsqrt<-sqrt(1/nrow(Td$smat)+1/ncol(Td$smat))
    sdsqrt<-fsqrt*sdnoise
    sigorig<-sd(Td_noise$smat, na.rm = TRUE)

    lm.ls_vs_sig <- lm(c(Td_noise$l.s.x) ~ c(Td$smat), na.action=na.exclude)
    b<-coef(lm.ls_vs_sig)[1]; a<-coef(lm.ls_vs_sig)[2]
    cat("\nLeast squared vs signal linear fit \n")
    #print(summary(lm.ls_vs_sig))
    #a<-(0.887)^2  ; b<- 0.54 ;storig<-2.42; sig0<-0.67;
    ms<-mean(Td$smat, na.rm=TRUE)
    #sdcalc<-sqrt((a-1)^2*sigorig^2+(a^2)*(sdsqrt)^2+b^2  +(a-1)*b*ms)
    #sdcalc<-sqrt((a-1)^2*sigorig^2+(a^2)*(sdsqrt)^2+b^2  +(a-1)*b*ms)   # pre feb 2019 version
    sdcalc<-sqrt((a-1)^2*(sigorig^2+ms^2) +2*(a-1)*b*ms+(a^2)*(sdsqrt)^2+b^2 )
    noisedata<-Td_noise$smat-Td$ET.x
    cnoise<-rowMeans(noisedata, na.rm = TRUE)
    rnoise<-colMeans(noisedata, na.rm = TRUE)
    noiseavedata<-noisedata;noiseavedata[]<-NA
    for(c in 1:ncol(noisedata))for(r in 1:nrow(noisedata))noiseavedata[r,c]<-cnoise[c]*rnoise[r]
    sdnoiseav<-sd(noiseavedata, na.rm = TRUE)
    sdcalc1<-sqrt((a-1)^2*(sigorig^2+ms^2) +2*(a-1)*b*ms+(a^2)*(sdnoiseav)^2+b^2 )

    sdall<-list(rmult=rmult,cmult=cmult,nr=nrow(Td$smat),nc=ncol(Td$smat),sdnoise=sdnoise,
                sdE= sdE,sdls=sdls,sdEave=sdEave,fsqrt= fsqrt,sdsqrt=sdsqrt,sdcalc=sdcalc,sdcalc1=sdcalc1)
    return(sdall)
  },cmult=cmult, eg=actualFunction,simplify = TRUE,diagonal=TRUE )
  sdx<-matrix(as.numeric(sdx), nrow=nrow(sdx),ncol=ncol(sdx),
              dimnames=list(rownames(sdx),colnames(sdx)))
  plot(sdx["nr",], sdx["sdnoise",],ylim=c(0,max(sdx["sdnoise",])),
       main=paste0("squares=Equitable,ls=Stars,noise=circles\nJitter: line= lowest std dev \n",
                   "cmult= ", cmult))
  lines(sdx["nr",], sdx["sdsqrt",])
  lines(sdx["nr",], sdx["sdls",],type="p",pch=11)
  #lines(sdx["nr",], sdx["fsqrt",])
  lines(sdx["nr",], sdx["sdE",],type="p",pch=15)
  lines(sdx["nr",], sdx["sdcalc",])
  lines(sdx["nr",], sdx["sdcalc1",])
  #lines(sdx["nr",], sdx["sdEave",],type="p",pch="O")
  cat("\n",sdx)
  print(sdx)
}#run jitter noise levels(c*std dev) from c=start,end by inc for actualfunction=eg1,eg2,..etc and plot
runsdnormal<-function(start, end, cmult,fac,
                      actualFunction,Ave=TRUE,diagonal=TRUE,pf=FALSE,ipf=FALSE,C=1.028){
  sdx<-sapply(start:end, FUN=function(rmult,cmult,fac,eg,Ave,diagonal,ipf,pf,C){

    Td<-Tnorm(rmult,cmult,fac,noise=FALSE, FUN=eg,Ave=Ave,diagonal=diagonal)
    Td_noise<-Tnorm(rmult,cmult,fac, FUN=eg,Ave=Ave,diagonal=diagonal)
    runstatsNS(Td,Td_noise)
    sdE<-sd(Td_noise$ET.x-Td$ET.x, na.rm = TRUE)
    sdls<-sd(Td_noise$l.s.x-Td$ET.x, na.rm = TRUE)
    sdEave<-sd(Td_noise$Ave.ET.x-Td$ET.x, na.rm = TRUE)
    sdnoise<-sd(Td_noise$smat-Td$ET.x, na.rm = TRUE)
    fsqrt<-sqrt(1/nrow(Td$smat)+1/ncol(Td$smat))
    sdsqrt<-fsqrt*sdnoise
    sigorig<-sd(Td_noise$smat, na.rm = TRUE)

    lm.ls_vs_sig <- lm(c(Td_noise$l.s.x) ~ c(Td$smat), na.action=na.exclude)
    b<-coef(lm.ls_vs_sig)[1]; a<-coef(lm.ls_vs_sig)[2]
    cat("\nLeast squared vs signal linear fit \n")
    #print(summary(lm.ls_vs_sig))
    #a<-(0.887)^2  ; b<- 0.54 ;storig<-2.42; sig0<-0.67;

    ms<-mean(Td$smat, na.rm=TRUE)
    #sdcalc<-sqrt((a-1)^2*sigorig^2+(a^2)*(sdsqrt)^2+b^2  +(a-1)*b*ms)
    #sdcalc<-sqrt((a-1)^2*sigorig^2+(a^2)*(sdsqrt)^2+b^2  +(a-1)*b*ms)   # pre feb 2019 version
    sdcalc<-sqrt((a-1)^2*(sigorig^2+ms^2) +2*(a-1)*b*ms+(a^2)*(sdsqrt)^2+b^2 )
    noisedata<-Td_noise$smat-Td$ET.x
    cnoise<-rowMeans(noisedata, na.rm = TRUE)
    rnoise<-colMeans(noisedata, na.rm = TRUE)
    noiseavedata<-noisedata;noiseavedata[]<-NA
    for(c in 1:ncol(noisedata))for(r in 1:nrow(noisedata))noiseavedata[r,c]<-cnoise[c]*rnoise[r]
    sdnoiseav<-sd(noiseavedata, na.rm = TRUE)
    sdcalc1<-sqrt((a-1)^2*(sigorig^2+ms^2) +2*(a-1)*b*ms+(a^2)*(sdnoiseav)^2+b^2 )

    stdvalues<-stats_residuals(Td_noise,Td=Td,ipf=ipf,pf=pf,C=C)

    sdall<-list(rmult=rmult,cmult=cmult,nr=nrow(Td$smat),nc=ncol(Td$smat),sdnoise=sdnoise,
                theory_sdN_Ave_plus  =stdvalues$theory_sdN_Ave_plus, sdnoise_approx = stdvalues$sdnoise_approx ,theory_sdN_Ave=stdvalues$theory_sdN_Ave,
                sdE= sdE,sdls=sdls,sdEave=sdEave,fsqrt= fsqrt,sdsqrt=sdsqrt,sdcalc=sdcalc,sdcalc1=sdcalc1,sdN_Ave=stdvalues$sdN_Ave)
    return(sdall)
  },cmult=cmult,fac=fac, eg=actualFunction,simplify = TRUE,Ave=Ave,diagonal=diagonal,ipf=ipf,pf=pf,C=C )
  sdx<-matrix(as.numeric(sdx), nrow=nrow(sdx),ncol=ncol(sdx),
              dimnames=list(rownames(sdx),colnames(sdx)))
  plot(sdx["nr",], sdx["sdnoise",],ylim=c(0,max(sdx["sdnoise",])),ylab="Standard Deviation",xlab="Number of Rows",
       main=paste0("Standard deviation vs Number of Rows \n",
                   " cmult= ", cmult," std fac=",fac," C=",C),pch=16,cex=1.5,cex.lab=1.5)
  pch<-t((c(16,15,17,NA,NA,NA,NA,NA)))
  legend<-c('Noise:I-S','T-S','L.S.-S','Row/Col Ave Noise',"Predicted:L.S.(Bias)","Predicted:L.S.(Bias1)", "Predicted Noise","Predicted:T-S","True Noise Ave" )
  lty<-c(NA,NA,NA,1,3,3,   4,5,3)
  lwd<-c(NA,NA,NA,4,2,3,   3,2,4)
  legend('bottomleft',inset=.00,legend=legend,
         lwd=lwd,lty=lty,pch = pch,bg='white',ncol=c(2),cex=0.75)

  #lines(sdx["nr",], sdx["sdsqrt",],lty=1,lwd=4)
  lines(sdx["nr",], sdx["theory_sdN_Ave",],lty=1,lwd=4)
  lines(sdx["nr",], sdx["sdls",],type="p",pch=17,cex=1.5)
  #lines(sdx["nr",], sdx["fsqrt",],lty=1,lwd=4)
  lines(sdx["nr",], sdx["sdE",],type="p",pch=15,cex=1.5)
  lines(sdx["nr",], sdx["sdcalc",],lty=3,lwd=2)
  lines(sdx["nr",], sdx["sdcalc1",],lty=3,lwd=3)
  lines(sdx["nr",], sdx["sdnoise_approx",],lty=4,lwd=4)
  lines(sdx["nr",], sdx["theory_sdN_Ave_plus",],lty=5,lwd=2)
  lines(sdx["nr",], sdx["sdN_Ave",],lty=3,lwd=4)
  #lines(sdx["nr",], sdx["sdEave",],type="p",pch=24)
  print(sdx)
  return(sdx)
}#run normal dis. noise levels(c*std dev) from c=start to end by inc for actualfunction=eg1,eg2,..etc and plot

runsdnormalrmult<-function(start, end, rmult,fac,
                      actualFunction,Ave=TRUE,diagonal=TRUE,pf=FALSE,ipf=FALSE,C=1.028){
  sdx<-sapply(start:end, FUN=function(rmult,cmult,fac,eg,Ave,diagonal,ipf,pf,C){

    Td<-Tnorm(rmult,cmult,fac,noise=FALSE, FUN=eg,Ave=Ave,diagonal=diagonal)
    Td_noise<-Tnorm(rmult,cmult,fac, FUN=eg,Ave=Ave,diagonal=diagonal)
    runstatsNS(Td,Td_noise)
    sdE<-sd(Td_noise$ET.x-Td$ET.x, na.rm = TRUE)
    sdls<-sd(Td_noise$l.s.x-Td$ET.x, na.rm = TRUE)
    sdEave<-sd(Td_noise$Ave.ET.x-Td$ET.x, na.rm = TRUE)
    sdnoise<-sd(Td_noise$smat-Td$ET.x, na.rm = TRUE)
    fsqrt<-sqrt(1/nrow(Td$smat)+1/ncol(Td$smat))
    sdsqrt<-fsqrt*sdnoise
    sigorig<-sd(Td_noise$smat, na.rm = TRUE)

    lm.ls_vs_sig <- lm(c(Td_noise$l.s.x) ~ c(Td$smat), na.action=na.exclude)
    b<-coef(lm.ls_vs_sig)[1]; a<-coef(lm.ls_vs_sig)[2]
    cat("\nLeast squared vs signal linear fit \n")
    #print(summary(lm.ls_vs_sig))
    #a<-(0.887)^2  ; b<- 0.54 ;storig<-2.42; sig0<-0.67;

    ms<-mean(Td$smat, na.rm=TRUE)
    #sdcalc<-sqrt((a-1)^2*sigorig^2+(a^2)*(sdsqrt)^2+b^2  +(a-1)*b*ms)
    #sdcalc<-sqrt((a-1)^2*sigorig^2+(a^2)*(sdsqrt)^2+b^2  +(a-1)*b*ms)   # pre feb 2019 version
    sdcalc<-sqrt((a-1)^2*(sigorig^2+ms^2) +2*(a-1)*b*ms+(a^2)*(sdsqrt)^2+b^2 )
    noisedata<-Td_noise$smat-Td$ET.x
    cnoise<-rowMeans(noisedata, na.rm = TRUE)
    rnoise<-colMeans(noisedata, na.rm = TRUE)
    noiseavedata<-noisedata;noiseavedata[]<-NA
    for(c in 1:ncol(noisedata))for(r in 1:nrow(noisedata))noiseavedata[r,c]<-cnoise[c]*rnoise[r]
    sdnoiseav<-sd(noiseavedata, na.rm = TRUE)
    sdcalc1<-sqrt((a-1)^2*(sigorig^2+ms^2) +2*(a-1)*b*ms+(a^2)*(sdnoiseav)^2+b^2 )

    stdvalues<-stats_residuals(Td_noise,Td=Td,ipf=ipf,pf=pf,C=C)

    sdall<-list(rmult=rmult,cmult=cmult,nr=nrow(Td$smat),nc=ncol(Td$smat),sdnoise=sdnoise,
                theory_sdN_Ave_plus  =stdvalues$theory_sdN_Ave_plus, sdnoise_approx = stdvalues$sdnoise_approx ,theory_sdN_Ave=stdvalues$theory_sdN_Ave,
                sdE= sdE,sdls=sdls,sdEave=sdEave,fsqrt= fsqrt,sdsqrt=sdsqrt,sdcalc=sdcalc,sdcalc1=sdcalc1)
    return(sdall)
  },rmult=rmult,fac=fac, eg=actualFunction,simplify = TRUE,Ave=Ave,diagonal=diagonal,ipf=ipf,pf=pf,C=C )
  sdx<-matrix(as.numeric(sdx), nrow=nrow(sdx),ncol=ncol(sdx),
              dimnames=list(rownames(sdx),colnames(sdx)))
  plot(sdx["nc",], sdx["sdnoise",],ylim=c(0,max(sdx["sdnoise",])),ylab="Standard Deviation",xlab="Number of Columns",
       main=paste0("Standard deviation vs Number of Columns \n",
                   " rmult= ", rmult," std fac=",fac," C=",C))
  pch<-t((c(1,15,11,NA,NA,NA,NA)))
  legend<-c('Noise:I-S','T-S','L.S.-S','Row/Col Ave Noise',"Predicted:L.S.(Bias)","Predicted:L.S.(Bias1)", "Predicted Noise","Predicted:T-S" )
  lty<-c(NA,NA,NA,1,3,3,   4,5)
  lwd<-c(NA,NA,NA,4,2,3,   3,2)
  legend('bottomleft',inset=.00,legend=legend,
         lwd=lwd,lty=lty,pch = pch,bg='white',ncol=c(2),cex=0.75)

  #lines(sdx["nc",], sdx["sdsqrt",],lty=1,lwd=4)
  lines(sdx["nc",], sdx["theory_sdN_Ave",],lty=1,lwd=4)
  lines(sdx["nc",], sdx["sdls",],type="p",pch=11)
  #lines(sdx["nc",], sdx["fsqrt",],lty=1,lwd=4)
  lines(sdx["nc",], sdx["sdE",],type="p",pch=15)
  lines(sdx["nc",], sdx["sdcalc",],lty=3,lwd=2)
  lines(sdx["nc",], sdx["sdcalc1",],lty=3,lwd=3)
  lines(sdx["nc",], sdx["sdnoise_approx",],lty=4,lwd=4)
  lines(sdx["nc",], sdx["theory_sdN_Ave_plus",],lty=5,lwd=2)
  #lines(sdx["nc",], sdx["sdEave",],type="p",pch=24)
  print(sdx)
  return(sdx)
}#run normal dis. no

runsdnormalcol<-function(start, end, cmult,fac,
                         actualFunction){
  sdx<-sapply(start:end, FUN=function(rmult,cmult,fac,eg,diagonal){

    Td<-Tnormcol(rmult,cmult,fac,noise=FALSE, FUN=eg,diagonal=diagonal)
    Td_noise<-Tnormcol(rmult,cmult,fac, FUN=eg,diagonal=diagonal)
    runstatsNS(Td,Td_noise)
    sdE<-sd(Td_noise$ET.x-Td$ET.x, na.rm = TRUE)
    sdls<-sd(Td_noise$l.s.x-Td$ET.x, na.rm = TRUE)
    sdEave<-sd(Td_noise$Ave.ET.x-Td$ET.x, na.rm = TRUE)
    sdnoise<-sd(Td_noise$smat-Td$ET.x, na.rm = TRUE)
    fsqrt<-sqrt(1/nrow(Td$smat)+1/ncol(Td$smat))
    sdsqrt<-fsqrt*sdnoise
    sigorig<-sd(Td_noise$smat, na.rm = TRUE)

    lm.ls_vs_sig <- lm(c(Td_noise$l.s.x) ~ c(Td$smat), na.action=na.exclude)
    b<-coef(lm.ls_vs_sig)[1]; a<-coef(lm.ls_vs_sig)[2]
    cat("\nLeast squared vs signal linear fit \n")
    #print(summary(lm.ls_vs_sig))
    #a<-(0.887)^2  ; b<- 0.54 ;storig<-2.42; sig0<-0.67;
    ms<-mean(Td$smat, na.rm=TRUE)
    #sdcalc<-sqrt((a-1)^2*sigorig^2+(a^2)*(sdsqrt)^2+b^2  +(a-1)*b*ms)
    #sdcalc<-sqrt((a-1)^2*sigorig^2+(a^2)*(sdsqrt)^2+b^2  +(a-1)*b*ms)   # pre feb 2019 version
    sdcalc<-sqrt((a-1)^2*(sigorig^2+ms^2) +2*(a-1)*b*ms+(a^2)*(sdsqrt)^2+b^2 )
    noisedata<-Td_noise$smat-Td$ET.x
    cnoise<-rowMeans(noisedata, na.rm = TRUE)
    rnoise<-colMeans(noisedata, na.rm = TRUE)
    noiseavedata<-noisedata;noiseavedata[]<-NA
    for(c in 1:ncol(noisedata))for(r in 1:nrow(noisedata))noiseavedata[r,c]<-cnoise[c]*rnoise[r]
    sdnoiseav<-sd(noiseavedata, na.rm = TRUE)
    sdcalc1<-sqrt((a-1)^2*(sigorig^2+ms^2) +2*(a-1)*b*ms+(a^2)*(sdnoiseav)^2+b^2 )

    sdall<-list(rmult=rmult,cmult=cmult,nr=nrow(Td$smat),nc=ncol(Td$smat),sdnoise=sdnoise,
                sdE= sdE,sdls=sdls,sdEave=sdEave,fsqrt= fsqrt,sdsqrt=sdsqrt,sdcalc=sdcalc,sdcalc1=sdcalc1)
    return(sdall)
  },cmult=cmult,fac=fac, eg=actualFunction,simplify = TRUE,diagonal=FALSE )
  sdx<-matrix(as.numeric(sdx), nrow=nrow(sdx),ncol=ncol(sdx),
              dimnames=list(rownames(sdx),colnames(sdx)))
  plot(sdx["nr",], sdx["sdnoise",],ylim=c(0,max(sdx["sdnoise",])),
       main=paste0("squares=Equitable,ls=Stars,noise=circles\nNormal col: line= lowest std dev \n",
                   "cmult= ", cmult," std fac=",fac))
  lines(sdx["nr",], sdx["sdsqrt",])
  lines(sdx["nr",], sdx["sdls",],type="p",pch=11)
  #lines(sdx["nr",], sdx["fsqrt",])
  lines(sdx["nr",], sdx["sdE",],type="p",pch=15)
  lines(sdx["nr",], sdx["sdcalc",])
  lines(sdx["nr",], sdx["sdcalc1",])
  lines(sdx["nr",], sdx["sdEave",],type="p",pch="O")
  print(sdx)
  return(sdx)
}#run normalcol dis. noise levels(c*std dev) from c=start, end by inc for actualfunction=eg1,eg2,..etc and plot
twovarequit<-function(a,at){
# s[i,j]<- s<-uniroot(function(x) x^4-a[i,j]*x^3+a[j,i]*x-1, c(a[i,j]-?,a[i,j]+?))
ma<-max(abs(a),abs(at))

#v<-uniroot(function(x) x^4-a*x^3+at*x-1, c(0,ma))
z<-c(-1,at,-a,1)
#z<-c(-1,a,-a,1)
#z<-c(-1,at,-at,1)
sr<-polyroot(z)
 cat("\n",sr,"\n")
# cat("\n",Im(sr),"\n")
# cat("\n ",which(abs(Im(sr))<1e-10))
realroots<-Re(sr[which(abs(Im(sr))<1e-10)])
cat("\nreal roots",realroots)
min(realroots-a)
s<-realroots[which((realroots-a)==min(realroots-a))]
 st<-1/s[1]
slope<-list(s=s,st=st)
cat("\n slope=",slope$s,slope$st)
z<-c(-1,a,-at,1)
sr<-polyroot(z)
# cat("\n",sr,"\n")
# cat("\n",Im(sr),"\n")
# cat("\n ",which(abs(Im(sr))<1e-10))
realroots<-Re(sr[which(abs(Im(sr))<1e-10)])
#cat("\nreal roots",realroots)
min(realroots-at)
s<-realroots[which((realroots-at)==min(realroots-at))]
st<-1/s[1]
cat("\n reverse s,st",s,st,"\n")

return(slope)
} #find roots that give equitable slopes from regression slopes



eqfit<-function(N,M,fac=1){
strue<-runif(N, min = -10, max = 10)
strue
#sample(-10:10, 20, replace=TRUE)
x<-seq(1, M)
y<-strue[1]*x

  sdd<-sd(x,na.rm=TRUE)
  xn<-x+rnorm(M,mean=0,sd=fac*sdd)    #normal distribution with std dev of fac*d's std dev
  yn<-y+rnorm(M,mean=0,sd=fac*sdd)
  fit<-lm(yn~xn, na.action=na.exclude )
  fit_coef<-coef(summary(fit))

    a=fit_coef[2,"Estimate"]
    sse=fit_coef[2,"Std. Error"]
    b=fit_coef["(Intercept)","Estimate"]
    bse=fit_coef["(Intercept)","Std. Error"]
    r2=summary(fit)$r.squared
    N=summary(fit)$df[2]+2
    pslope=fit_coef[2,"Pr(>|t|)"]
 at<-r2/a
    cat("\ntrue slopes=",strue,1/strue," fitted slopes=",a,at, "r2= ",r2 )
    slopes<-twovarequit(a,at)
    lss<-c(1,a,at,1); lss<-matrix(lss,nrow=2,ncol=2)
    bt<-(-1)*at*b
     lsb<-c(0,b,bt,0); lsb<-matrix(lsb,nrow=2,ncol=2)
     E.s<- c(1,slopes$s,slopes$st,1); E.s<-matrix(E.s,nrow=2,ncol=2)
    # lsb<-c(0,bt,b,0); lsb<-matrix(lsb,nrow=2,ncol=2)
    # E.s<- c(1,slopes$s,slopes$st,1); E.s<-matrix(E.s,nrow=2,ncol=2)

    ps<-c(0,pslope,pslope,0); ps<-matrix(ps,nrow=2,ncol=2)
    mat<-list(s=lss,b=lsb, E.s=E.s,pslope=ps)
    Eb<-equitableb(mat)
    bnew<-Eb$b[2,1] ;
    cat("\n oldb =",b," new b=",bnew)
    #need new b also
    yeq<-slopes$s*xn+bnew
    yls<-a*xn+b
    plot(xn,yn, main=" squares=eq,3=triangles=ls")
    lines(xn,yeq, type="p",pch=15)
    lines(xn,yls, type="p",pch=2)
    lines(x,y, type="o",pch=1)
}   #generate fits and find roots to compare to regression slopes :usually similar sometimes way off (could be the transpose slope)

#' Construct equitable slope matrix from f(x) : image is displayed of matrix
#'
#' Given f(x) assumed to be defined relative to x=xref, an equitable matrix is constructed
#'
#' @param xref 1 (default) x reference value around which the matrix is constructed
#' @param f    a vector f(x) around which the equitbel matirx is constructed
#'
#' @return Equitable matrix
#'
#' @examples
#' make_A(xref=1,f=c(1,2,3))  #simple example of equitable matrix
#' make_A(xref=3,f=c(1,2,3))  #reference not important
#' plot(eg4(3,3)[,1])
#' A<-make_A(xref=1,f=eg4(3,3)[,1])
#' #more complicated matrix based on large scale wave f(1)g(t)  in eg4
#' A<-make_A(xref=2,f=eg4(3,3)[20,])
#'  #more complicated matrix based on large scale wave f(x)g(20) in eg4
#' A<-make_A(f=eg5(6,6)[,1])  #more complicated matrix based on small scale wave f(1)g(t)  in eg5
#' plot(eg5(10,10)[20,],type="b")
#'  #more complicated matrix based on small scale wave f(x)g(20) in eg5
#' A<-make_A(xref=2,f=eg5(10,10)[20,])
#'  #low resolution  matrix based on small scale wave f(x)g(20) in eg5
#' A<-make_A(xref=2,f=eg5(2,2)[20,])
#' #more complicated matrix based on small scale wave f(20)g(t) in eg7
#' A<-make_A(xref=2,f=eg7(10,10)[,20])
#' #more complicated matrix based on small scale wave f(x)g(20) in eg7
#' A<-make_A(xref=2,f=eg7(10,10)[20,])
#'
#'
#' @export
make_A<-function(xref=1,f){
  A<- matrix(NA,nrow=length(f), ncol=length(f))

  f[abs(f)<1e-10]<-1e-10
  A[,xref]<-sapply(1:nrow(A),function(r){f[r]/f[xref]})
  A<-sapply(1:ncol(A),function(c){A[,c]<-A[,xref]/A[c,xref]})  # plot(A[,xref])
  #imagenan(A,main="slope matrix from f",zlim=c(-2,2)) #A[1,1]
  sdA<-sd(A,na.rm=TRUE)
  mA<-mean(A,na.rm=TRUE)
  zlim<-c(mA-0.5*sdA,mA+0.5*sdA)
  imagenan(A,main="slope matrix from f",zlim=zlim)
  return(A)
} #makes equitbale matrix from f existing at col xref



Txt_from_axr_Irt_bxr<-function(axr,Irt,bxr=NULL,zero=0,maxA=NULL){
  #axr varies over a, Irt varies over t , bxr varies over x
  #need to not run slope when larger than 3?
  if(!is.null(maxA)) axr[axr>maxA]<-NA
  if(is.null(bxr)){bx<-rep(0,length(axr))}
  Ixt<-matrix(NA,nrow=length(Irt),ncol=length(axr))
  colnames(Ixt)<-names(axr)
  rownames(Ixt)<-names(Irt)
 for(t in 1:length(Irt)){Ixt[t,]<-axr*(Irt[t]-zero)+bxr}
    imagenan(Ixt,main="Data built from a(x,r), b(x,r), I(r,t)")
    Ixt<-Ixt+zero
  return(Ixt)
}

# axr<-Tave$E.s[,"Row_Ave"]
# bxr<-Tave$E.b[,"Row_Ave"]
# Irt<-Tave$smat[,"Row_Ave"]
# zero<-Tave$l.s.zero
# Txt<-Txt_from_axr_Irt_bxr(axr=axr,Irt=Irt,bxr=bxr,zero=zero,maxA=3)

make_Amod<-function(xref=1,f,modv=NULL,addflag=FALSE,origflag=FALSE,ident=NULL,roundflag=TRUE,pflag=TRUE){    #f<-c(1,2,3,4)  ;modv<-5 addflag<-TRUE
  if(is.null(modv)){
    if(addflag)modv<-length(f) else modv<-length(f)+1
    cat("\nsystem is mod ",modv,"\n")
  }
  A<-A1<-Anomod<- A1nomod<-matrix(NA,nrow=length(f), ncol=length(f))

  f[abs(f)<1e-10]<-1e-10
  if(addflag){
    A[,xref]<-sapply(1:nrow(A),function(r){(f[r])%%modv})
    A<-sapply(1:nrow(A),function(c){A[,c]<-(f+f[c])%%modv})  # plot(A[,xref])
    Anomod[,xref]<-sapply(1:nrow(Anomod),function(r){(f[r])})
    Anomod<-sapply(1:nrow(Anomod),function(c){Anomod[,c]<-(f+f[c])})  # plot(A[,xref])
  } else{
  A[,xref]<-sapply(1:nrow(A),function(r){(f[r])%%modv}) #sapply(1:nrow(A),function(r){(f[r])})
  A<-sapply(1:nrow(A),function(c){A[,c]<-((f*f[c])%%modv)})  # plot(A[,xref])   (Amult%*%Aadd)%%modv (Amult%*%Aadd)%%(modv-1)
  Anomod[,xref]<-sapply(1:nrow(Anomod),function(r){(f[r]/f[xref])}) #sapply(1:nrow(A),function(r){(f[r])})
  Anomod[xref,]<-sapply(1:nrow(Anomod),function(r){(f[xref]/f[r])})
  Anomod<-sapply(1:nrow(Anomod),function(c){Anomod[,c]<-Anomod[,xref]*Anomod[xref,c]})

   }
  if(roundflag)A<-round(A)
  imagenan(Anomod,main="slope matrix No modulus  from f") #A[1,1]
  imagenan(A,main=" slope matrix from f") #A[1,1]
  if(!origflag){
    if(is.null(ident)){
  if(addflag  ) ident<-0 else ident<-1
  }
  for( c in 1:ncol(A)){
    A1[,c]<-A[,which(A[,c]==ident)[1]]
    A1nomod[,c]<-Anomod[,which(A[,c]==ident)[1]]
  }
  A<-A1
  Anomod<-A1nomod
  }
  if(roundflag)A<-round(A)
  sdA<-sd(A,na.rm=TRUE)
  mA<-mean(A,na.rm=TRUE)
  zlim<-c(mA-0.5*sdA,mA+0.5*sdA)
  if(pflag)imagenan(Anomod,main="slope matrix from f")
  if(pflag)imagenan(A,main="slope matrix from f",zlim=zlim)
  return(A)
} #makes equitable matrix from f existing at col xref

#' Construct Shift equitable matrix
#'
#' Finds a shift equitbale matrix from a slope equitbel matrix and a sequence profile
#'
#' @param xref  Reference "location" to base shift matrix default set to 1 (for PCA dominant is first)
#' @param orig  2D Data set used when uflag FALSE   profile at t=constant when uflag=TRUE  (u(x) equivalent)
#' @param A     Slope equitable matrix
#' @param uflag  TRUE(default) or FALSE   TRUE uses origas u(x): FALSE uses average profile (averaged over rows)
#' @param pflag  ignore
#'
#' @return B shift matrix B
#'
#' @examples
#' d<-eg4(2,2); Td<-transformE(d)
#' B<-make_B(orig=Td$ET.x, A=Td$E.s)
#' B<-make_B(xref=5,orig=Td$ET.x, A=Td$E.s)
#' B<-make_B(xref=5,orig=Td$ET.x[1,], A=Td$E.s,uflag=TRUE)
#'
#' @export
make_B<-function(xref=1,orig,A,uflag=FALSE,pflag=TRUE){   #colnames(Td$ET.x)
  if(is.null(xref))xref<-ncol(A)
  B<- matrix(NA,nrow=nrow(A), ncol=ncol(A))
  if(!uflag) {cme<-colMeans(orig,na.rm=TRUE) } else cme<-orig  #orig=I[ tref,]
  plot(cme,pch=15,type="b",main=paste("Time averaged column variation",xref))  #imagenan(cm)
  #plot(cm)
  # Itave<-t(matrix(rep(cm,nrow(orig)),nrow=ncol(orig),ncol=nrow(orig)))  ;imagenan(mcm)
  # newd<-orig-Itave;
  B[,xref]<- cme-A[,xref]*cme[xref]  #plot(B[,xref])    plot(A[,xref]) plot(cme)
  B<-sapply(1:ncol(B),function(c){B[,c]<-B[,xref]-A[,c]*B[c,xref]})
  sdB<-sd(B,na.rm=TRUE)
  mB<-mean(B,na.rm=TRUE)
  zlim<-c(mB-1.*sdB,mB+1.*sdB)   #plot(B[,xref])
  if(pflag) imagenan(B,main=paste("intercept matrix from Time Average and slope\nxref=",xref),zlim=zlim)
  return(B)
} #make intercept matrix from time average or u if uflag=TRUE) assumed to be at xref


make_Bmod<-function(xref=1,orig=u,A=A,uflag=TRUE,modv=NULL,addflag=FALSE,origflag=FALSE,ident=NULL,opswitch=FALSE) {
  if(is.null(modv)){
    if(addflag)modv<-ncol(A) else modv<-ncol(A)+1
    cat("\nsystem is mod ",modv,"\n")
  }
  B<- matrix(NA,nrow=nrow(A), ncol=ncol(A))
  if(!uflag) cme<-colMeans(orig,na.rm=TRUE) else cme<- orig
  plot(cme,pch=15,type="b",main="Time averaged column variation")  #imagenan(cm)
  #plot(cm)
  # Itave<-t(matrix(rep(cm,nrow(orig)),nrow=ncol(orig),ncol=nrow(orig)))  ;imagenan(mcm)
  # newd<-orig-Itave;
  if(opswitch){
    B[,xref]<- (cme/A[,xref]+cme[xref])%%modv #plot(B[,xref])    plot(A[,xref]) plot(cme)
    B<-sapply(1:ncol(B),function(c){B[,c]<-(B[,xref]/A[,c]+B[c,xref])%%modv})
  } else{
  B[,xref]<- (cme-A[,xref]*cme[xref])%%modv #plot(B[,xref])    plot(A[,xref]) plot(cme)
  B<-sapply(1:ncol(B),function(c){B[,c]<-(B[,xref]-A[,c]*B[c,xref])%%modv})
  }
  sdB<-sd(B,na.rm=TRUE)
  mB<-mean(B,na.rm=TRUE)
  zlim<-c(mB-1.*sdB,mB+1.*sdB)   #plot(B[,xref])
  #imagenan(B,main="intercept matrix from Time Average and slope",zlim=zlim)
  return(B)
} #mak

#' Builds a simple (unweighted) equitable transform.
#'
#' It is based on equitable slope (A) and  shift (B) matrices and data set I
#' Both A and B can be tored efficiently by only retaining 1 column from each matrix
#' NA values are allowed
#' If a transform exists, this function can construct a complete data set from either:
#' 1. a complete data set
#' 2. A data set of only an average profile in variable t or a profile for some individual or location
#' 3. profile at one rbitrary location is available
#' 4. scattered observations across the different "individuals" or locations x for different values of t
#' Examples of each are given.
#' Only 2*length of x dimension + length of t dimension are needed to reconstruct system
#' If the data is not precisely equitable then more data provides a better estimate.
#' # Errors are also given to show how closely the system matches the equitbale one that is assumed.
#'
#' @param A equitable slope matrix NxN
#' @param B equitable shift matrix MxM
#' @param I two dimensional data set  MxN that can contain many NA values
#' @param zero Value to be subtracted from I to generate B default NULL (0)
#' @param maxA  Values in the slope matrix larger than this threshold are not used in the transform Default NULL
#'
#' @return Equitable transform of data that fills in many of the NA values based on the tranformed values
#'
#' @examples
#' #case 1 not shown. It is less interesting unless it is a noisy or corrupted system
#' #case 2: Use an average profile with 2 vectors from the matrices to construct
#' #        transformed data. (2N+M data points)
#' # To illustrate the idea first use an an equitable data set to construct
#' #            equitable matrix A and shift matrix B
#' #from which vectors f an u are extracted. These are used to reconstruct the matrices when desired.
#'
#' d<-eg4(2,2); Td<-transformE(d)  # make a data set and find the transform and associated matrices
#' f<-Td$E.s[,1]                   # form
#' u<-Td$E.b[,1]
#' I<-Td$smat;I[]<-NA; I[,"Row_Ave"]<-Td$smat[,"Row_Ave"]
#' # the 3 necessary components are made so show what they look like and then construct system
#' plot(f,type="b",main="f(x) profile used for Equitable matrix A")
#' plot(u,type="b",main="u(x) profile used with matrix A\nto make  for shift matrix B")
#' plot(I[, "Row_Ave"],type="b",,main="Average profile used for reconstruction")
#' imagenan(I,main="Only the average profile is defined")
#' A<-make_A(f=f)
#' B<-make_B(xref=1,orig=u,A=A,uflag=TRUE)
#' TI<-AI_Bmult_NA(A=A,I=I,B=B)
#' attributes(TI)
#' imagenan(TI$xme,main="Reconstructed data set from Average profile")
#'          #data successfully reconstructed
#'
#' # case 3  profile at x=10 available but it is somewhat noisy
#' xref<-10; I<-Td$smat;I[]<-NA;
#' I[,xref]<-Td$smat[,xref]
#' plot(I[, xref],type="b",main="Profile (no Noise)")
#' I[,xref]<-I[,xref]+rnorm(nrow(I),mean=0,sd=(1/10*sd(I[,xref],na.rm=TRUE)))
#' plot(I[, xref],type="b",main="Profile used for reconstruction (with noise)")
#' imagenan(I,main=paste("data only from profile at location x=",xref))
#' TI<-AI_Bmult_NA(A=A,I=I,B=B)
#' attributes(TI)
#'imagenan(TI$xme,main=paste("Reconstructed data set from noisy profile at location x=",xref))

#' # case 3  profile at x=10 available but is completely different from original
#' xref<-10; I<-Td$smat;I[]<-NA;
#'
#' I[,xref]<-eg5(2,2)[,xref]
#' plot(I[, xref],type="b",main="Average profile used for reconstruction")
#' imagenan(I,main=paste("data only from profile at location x=",xref))
#' TI<-AI_Bmult_NA(A=A,I=I,B=B)
#' attributes(TI)
#'imagenan(TI$xme,main=paste("Reconstructed data set from profile at location x=",xref))
#'
#' #case 5 scattered information around the data set
#' #using random points over the data set only works
#' #if the system has the same equitable sense as the matrices
#' I<-Td$smat
#' numspaces<-nrow(I)
#' column_location<-sample(1:ncol(I), numspaces, replace=TRUE)
#' I_NA<-I; I_NA[]<-NA;g<-rep(NA,nrow(I))
#' for(t in 1:nrow(I_NA)){I_NA[t,column_location[t]]<-I[t,column_location[t]];
#'  g[t]<-I[t,column_location[t]]}
#' plot(g,type="b",main=paste("Data set from random points on x as function of t" ))
#' imagenan(I_NA,main=paste("Data set from random points on x" ))
#' A<-make_A(f=f)
#' B<-make_B(xref=1,orig=u,A=A,uflag=TRUE)
#' TI<-AI_Bmult_NA(A=A,I=I_NA,B=B)
#' attributes(TI)
#' imagenan(TI$xme,
#' main=paste("Data set reconstructed from random points on x as function of t" ))
#'
#' #case 5 scattered information around the data set that is noisy
#' #using random points over the data set assumes the system has
#' # the same equitable sense as the matrices
#' I<-Td$smat ;g<-rep(NA,nrow(I))
#' numspaces<-10*nrow(I) ;g<-rep(NA,numspaces)
#' column_location<-sample(1:(ncol(I)-1), numspaces, replace=TRUE)
#' I_NA<-I; I_NA[]<-NA;g<-rep(NA,nrow(I))
#' Ieg4<-eg4(2,2)
#' Ieg4<-Ieg4+  rnorm(prod(dim(Ieg4)),mean=0,sd=(1/10*sd(Ieg4,na.rm=TRUE)))
#' imagenan(Ieg4,main="Noise added")
#' for(t in 1:numspaces){
#'   I_NA[t%%nrow(I)+1,column_location[t]]<-Ieg4[t%%nrow(I)+1,column_location[t]]
#'                            g[t]<-Ieg4[t%%nrow(I)+1,column_location[t]]
#'                            }
#' imagenan(I_NA,main=paste("Data set from random points on x" ))
#' A<-make_A(f=f)
#' B<-make_B(xref=1,orig=u,A=A,uflag=TRUE)
#' TI<-AI_Bmult_NA(A=A,I=I_NA,B=B)
#' attributes(TI)
#' imagenan(TI$xme,
#' main=paste("Data set reconstructed from random points",
#'  "(from eg4 with noise) on x as function of t" ))
#' imagenan(TI$xsd,
#' main=paste("Std Dev in reconstruction from random points\n",
#' " (from eg4 with noise) on x as function of t" ))
#' #error values show where the system was not equitable
#'
#' @export
AI_Bmult_NA<-function(A,I,B=NULL,zero=0,maxA=NULL){
  I<-as.matrix(I-zero)
    #need to not run slope when larger than 3?
    if(!is.null(maxA)) A[A>maxA]<-NA
    if(is.null(B)){B<-matrix(0,nrow=nrow(A),ncol=ncol(A))}
    # ones<-Matrix(rep(1,ncol(I)*nrow(I)),nrow=nrow(I),ncol=ncol(I))
     #bval<-B1%*%ones/ncol(B1) # imagenan(bval);
    # Tsimple<-Tsimple1+bval + zero  #  imagenan(Td$E.b)

    AyI_By<-AyI_Byxme<-AyI_Byxsd<-AyI_ByN<-NULL
    for(t in 1:nrow(I)){
      It<-I[t,]
      xme<-xsd<-N<-NULL
    AIt_B<-sapply(1:ncol(A),function(y){
      xme<-mean((c(A[y,]*It+B[y,])),na.rm=TRUE)
      xsd<-sd(A[y,]*It+B[y,],na.rm=TRUE)
      N<-length(which(!is.na(A[y,]*It)))   #length(A[y,]) ; length(It)
      AIt_B<-list(xme=xme,xsd=xsd,N=N)
      return(AIt_B)
    })
    xme<-unlist(AIt_B["xme",]) #plot(xme)
    xsd<-unlist(AIt_B["xsd",])
    N<-unlist(AIt_B["N",])
    AyI_Byxme<-cbind(AyI_Byxme,xme)
    AyI_Byxsd<-cbind(AyI_Byxsd,xsd)
    AyI_ByN<-cbind(AyI_ByN,N)

    }
    rownames(AyI_Byxme)<-rownames(AyI_Byxsd)<-rownames(AyI_ByN)<-colnames(I)
    colnames(AyI_Byxme)<-colnames(AyI_Byxsd)<-colnames(AyI_ByN)<-rownames(I)
    AI_B<-list(xme=t(AyI_Byxme+zero),xsd=t(AyI_Byxsd),N=t(AyI_ByN)) #imagenan(AyI_Byxme)

    return(AI_B)
    # AI<-ABmult_NA(A=A,B=orig)  #imagenan(AI)
    # Tx<-AI$xme  #average,std dev and N  imagenan(Tx) ;imagenan(Tsd);imagenan(TN)
    # Tsd<-AI$xsd
    # TN<-AI$N
  }

make_data_fromE<-function(orig,A,B=NULL,zero=0,maxA=3, main=" "){
  orig<-as.matrix(orig)
 # if(is.null(B)){B<-matrix(0,nrow=nrow(A),ncol=ncol(A))}
   AI<-AI_Bmult_NA(A=A,I=orig,B=B,maxA=maxA)
  #imagenan(AI)#A<-Td1$E.s; B<-Td1$E.b;orig<-Td1$smat  ;orig[5:10,1:3]<-NA   ;imagenan(orig)
    Tx<-AI$xme  #attributes(AI) imagenan(Tx) ;imagenan(Tsd);imagenan(TN)
    Tsd<-AI$xsd
    TN<-AI$N               #imagenan(orig) ;imagenan(Tx) ;imagenan(Tsd);imagenan(TN)
    sdo<-sd(orig,na.rm=TRUE)
    mo<-mean(orig,na.rm=TRUE)
    zlim<-c(mo-1.5*sdo,mo+1.5*sdo)
    imagenan(Tx,main=paste(main,"Equitable"),zlim=zlim);
    imagenan(orig,main=paste(main,"Original"),zlim=zlim); cat("\nresidual std (Simple Transform-Orig)",main, sd(Tx-orig))
    imagenan(orig-Tx,main=paste(main,"Residuals"),zlim=zlim)
    plot(Tx,orig,main=paste(main," vs Original"))  ;lines(orig,orig)
    Tsimple<-list(Tx=Tx,Tsd=Tsd,TN=TN)
    return(Tsimple)

}#make data from simple slope intercept

frac_dim<-function(orig,meanflag=TRUE,refval=NULL,fracflag=FALSE,upperquant=NULL,main=""){
  main<-paste(main,"meanflag ",meanflag,"fracflag ",fracflag,"\n")
  orig[which(is.infinite(orig))]<-NA
  orig[which(is.nan(orig))]<-NA
  # maxI<-max(c(orig),na.rm=TRUE)
  # minI<-min(c(orig),na.rm=TRUE)
  if(is.null(upperquant)){
    maxI<-max(c(orig), na.rm=TRUE)
  minI<- min(c(orig), na.rm=TRUE)
  cat("\n",main,"\nusing min max as total range",minI,maxI)
  } else {
  maxI<-quantile(c(orig),probs=upperquant, na.rm=TRUE)
  minI<- quantile(c(orig),probs=(1-upperquant), na.rm=TRUE)  #quantile(c(0,1,2,3,4),0.8) quantile(I,probs=0.9,type=1) imagenan(I,type=1)
  cat("\n",main,"\nusing Quantiles",(1-upperquant),upperquant," as total range",minI,maxI)
  }
  if(meanflag){
    meanI<-mean(c(orig),na.rm=TRUE)

    if(is.null(refval)) {
      refval<-meanI
      cat("\n Using mean as refval ",meanI)
    } else cat("\n",main,"\n User refval used",refval)
    dmax1=abs(maxI-refval)
    dmax2=abs(refval-minI)
    if(dmax2<dmax1)dmax<-dmax1 else dmax<-dmax2
  } else {
    if(is.null(refval)){
      refval<-minI
      cat("\n",main,"\n Using min as refval ",minI)
    } else cat("\n User refval used",refval)
    dmax=maxI-refval
  }

  if(!is.na(dmax) && !is.nan(dmax) && !is.infinite(dmax) &&dmax>1e-10){   #imagenan(abs(orig-refval))
    if(fracflag){
      diff<-orig
      diff[which(orig<=refval)]<-0
      diff[which(orig>refval)]<-1
      imagenan(diff,main=paste(main,"Diff Values "),zlim=c(0.4,0.6))
    } else {
    diff<-abs(orig-refval)/dmax
    diff[which(diff>dmax)]<-1
    imagenan(diff,main=paste(main,"Diff Values "))
    }

  sum_delta<-sum(c(diff),na.rm=TRUE)
  sum_tot<-length(orig[which(!is.na(orig))])
  if(sum_delta>1e-10){
  dimen<- 2*log(sum_delta)/(log(sum_tot))
  if(fracflag) imagenan(diff,main=paste(main,"Diff Values Dimension=",round(dimen,digits=3)),zlim=c(0.4,0.6))
  else imagenan(diff,main=paste(main,"Diff Values Dimension=",round(dimen,digits=3)))
  } else dimen<-NA
  } else dimen<-NA

cat("\n",main,"\ndmax ",dmax," min",minI," max ",maxI," refval is ",refval," sum_delta",sum_delta," sum_tot",sum_tot,"\n",
    " T dimension is ",dimen,"\n")
indim<-list(dimen=dimen,diff=diff)
  return(indim)
}

#Tsimple<-make_data_fromE(orig=Td$smat,A=Td$E.s,B=Td$E.b,zero=Td$l.s.zero, main="Direct Simple ")#make data from simple slope intercept

# center with 'colMeans()'
center_colmeans <- function(x) {
  xcenter = colMeans(x,na.rm=TRUE)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
}

Addin_center_colmeans <- function(x,orig) {
  xcenter = colMeans(orig,na.rm=TRUE)
  x + rep(xcenter, rep.int(nrow(x), ncol(x)))
}

make_data_frompca<-function(orig,main=" ",pca_type=NULL){
  #orig should be centred version of data using orig<-center_colmeans(data)
  #then use Addin_center_colmeans(new)  to get back values
  orig<-as.data.frame(orig)
  #dat<-center_colmeans(orig)
  if(ncol(orig)>3){
    if(is.null(pca_type)){
    res.cov<-cov(orig)
    eig<-eigen(res.cov)
    } else{ #find eigen vectors using nipals
      #this doesnt work yet because loadings are not eigen vect
      imagenan(orig)
      m3 <- nipals(dat, gramschmidt=TRUE, center = FALSE,scale = FALSE)
      eig<-m3$loadings
      # library(nipals)
      # m3 <- nipals(dat, gramschmidt=TRUE, center = FALSE,scale = FALSE)
      # round(crossprod(m3$loadings),3)[1:5,1:5] # Prin Comps are orthogonal
      # imagenan(round(crossprod(m3$loadings),3)[1:5,1:5])
      # plot(m3$scores[,1]); plot(m3$loadings[,1])   #;plot((sqrt(abs(m3$scores[,1]))*m3$scores[,1]/abs(m3$scores[,1])))
      # #plot(m3$ncomp)
      # #dat_frompca<-m3$scores[,1]%*%t(m3$loadings[,1])  #attributes(m3);attributes(m3$eig)
      # dat_frompca<-m3$scores[,1:1]%*%t(m3$loadings[,1:1])    #%*%eig$vectors[,1:3];  plot(m3$scores[,1])
      # dat_frompca<-sd(dat,na.rm=TRUE)/sd(dat_frompca,na.rm=TRUE)*dat_frompca

      }
    imagenan(eig$vectors,main=paste("Eigenvectors",main))
    rownames(eig$vectors)<-colnames(orig)
    plot(eig$vectors[,1],type="b",pch=15,main="1st Principal Component Eigenvector")
    plot(eig$vectors[,2],type="b",pch=15,main="2nd Principal Component Eigenvector")  # colnames(Td2$E.s) colnames(Td2$smat)
    plot(eig$vectors[,3],type="b",pch=15,main="3rd Principal Component Eigenvector")

    newdata.t <- t(orig)
    # Transpose eigeinvectors
    eigenvectors.t <- t(eig$vectors)
    # The new dataset
    df.new <- eigenvectors.t %*% newdata.t  #looks like g(t)*sigma(f)^2
    # Transpose new data ad rename columns
    df.new <- t(df.new)
    colnames(df.new) <- paste0("PC",1:ncol(df.new)) #c("PC1", "PC2", "PC3", "PC4")
    imagenan(df.new[,1:3],main="1st 3 Principal Components (Equiv to slopes)")
    plot(df.new[,1],type="b",pch=15,main="1st Principal Component DATA:g1(t)*sigma(f1)^2")
    plot(df.new[,2],type="b",pch=15,main="2nd Principal Component DATA:g2(t)*sigma(f2)^2")  # colnames(Td2$E.s) colnames(Td2$smat)
    plot(df.new[,3],type="b",pch=15,main="3rd Principal Component DATA:g3(t)*sigma(f3)^2") #looks like scaled data

    # sapply(1:ncol(B),function(c){plot(Td$E.b[,c],main=paste(c))})

    # Tsimple<-make_data_fromE(orig=Td$smat,A=Td$E.s,B=Td$E.b,zero=Td$l.s.zero, main=" ")   #make data from simple slope intercept
    # zero=Td$l.s.zero
    # Torig1<-orig-t(Tsimple) #imagenan(Torig1,zlim=c(-0.1,0.1)); imagenan(orig); imagenan(e2+e3,zlim=c(-0.1,0.1))
    # sd(t(Tsimple)-Td$smat)
    #                                                #imagenan(as.matrix(Torig1),zlim=c(-0.1,0.1)) imagenan(Tsimple) imagenan(e1)
    # ETorig1<-orig-Td$ET.x   #imagenan(ETorig1,zlim=c(-0.2,0.2)); zlim<- c(min(orig-e1),max(orig-e1)) ;imagenan(orig-e1,zlim=zlim)
    eigenvect<-eigenvectors.t[1,]
    Tdata1<-make_datafrom_eigen(eigenvect= eigenvect,orig=orig,xref=1,main="1st Eigenvector") #imagenan(orig);imagenan(Tdata1) makedata from eigenvector and time average
    if(length(which(!is.na(Tdata1)))>0)Tx1<-Tdata1$Tx else Tx1<-NA
    eigenvect<-eigenvectors.t[2,]
    orig1<-orig-Tx1 #imagenan(orig1,zlim=c(-0.1,0.1));  imagenan(e2+e3,zlim=c(-0.1,0.1))

    Tdata2<-make_datafrom_eigen(eigenvect=eigenvect,orig=orig1,xref=2,sgn=(1),main="2nd Eigenvector")                  #makedata from eigenvector and time average
    if(length(which(!is.na(Tdata2)))>0)Tx2<-Tdata2$Tx else Tx2<-NA
    eigenvect<-eigenvectors.t[3,]
    orig2<-orig1-Tx2   #imagenan(Tdata2,zlim=c(-0.1,0.1))  ;imagenan(e2,zlim=c(-0.1,0.1))
                         # imagenan(orig2,zlim=c(-0.02,0.02));  imagenan(e3,zlim=c(-0.02,0.02))

    Tdata3<-make_datafrom_eigen(eigenvect=eigenvect,orig=orig2,xref=3,sgn=(1),main="3rd Eigenvector")                  #makedata from eigenvector and time average
    if(length(which(!is.na(Tdata3)))>0)Tx3<-Tdata3$Tx else Tx3<-NA
     Teigen<-list(e1=Tx1,e2=Tx2,e3=Tx3)   #E=Tsimple,
  }
  return(Teigen)
} #make data from simple T and 1st 3 eigenvectors

make3E<-function(x){
  Td1<-transformE(x,Ave=FALSE,diagonal=FALSE)
  plotsummary(Td1)
  x1<-Td1$smat-Td1$ET.x
  Td2<-transformE(x1,Ave=FALSE,diagonal=FALSE)
  plotsummary(Td2)
  x2<-Td2$smat-Td2$ET.x
  Td3<-transformE(x2,Ave=FALSE,diagonal=FALSE)
  plotsummary(Td3)
  x3<-Td3$smat-Td3$ET.x
  T3<-list(Td1=Td1,Td2=Td2,Td3=Td3,resid=x3)
  return(T3)
} #makes 3 transforms each from the residuals of the first  Td1=Td1,Td2=Td2,Td3=Td3,resid=x3

make3simpleE<-function(T3){
  Td<-T3$Td1
  Tx1<-make_data_fromE(orig=Td$smat,A=Td$E.s,B=Td$E.b,zero=Td$l.s.zero, main="Direct Simple 1")#
  #attributes(Tx1)
  Td<-T3$Td2
  Tx2<-make_data_fromE(orig=Td$smat,A=Td$E.s,B=Td$E.b,zero=Td$l.s.zero, main="Direct Simple 2")#

  Td<-T3$Td3
  Tx3<-make_data_fromE(orig=Td$smat,A=Td$E.s,B=Td$E.b,zero=Td$l.s.zero, main="Direct Simple 3")#
  Tx<-list(e1=Tx1$Tx,e2=Tx2$Tx,e3=Tx3$Tx)
  return(Tx)
} #

#make3simpleE(T3)

make_datafrom_eigen<-function(eigenvect,orig,xref=1,sgn=1,main=" "){
  orig<-as.matrix(orig)
  #   #make equitabe matrix from this eigenvector
f<-eigenvect

#plot(f);lines(eigenvect,type="b",pch="O")
cm<-colMeans(orig,na.rm=TRUE)  #;plot(cm)
mcm<-t(matrix(rep(cm,nrow(orig)),nrow=ncol(orig),ncol=nrow(orig)))  #;imagenan(mcm)
newd<-orig-mcm;#imagenan(newd); plot(f)

sdc<-sapply(1:ncol(newd),function(c){sd(newd[,c],na.rm=TRUE)})
F<-matrix(rep(f,length(f)),nrow=length(f),ncol=length(f))   #diag(F)
F<-sapply(1:ncol(F),function(c){F[,c]<-F[,c]/F[c,c]})   #imagenan(F)  imagenan(Td) plot(F[,1])  plot(f)
F[is.infinite(F)]<-NA
testd<- F%*%t(newd)/ncol(F)   # imagenan(t(testd));  imagenan(t(newd))  imagenan(t(testd)-newd)
sdc<-sapply(1:ncol(newd),function(c){
 sd(newd[,c]-t(testd)[,c],na.rm=TRUE)/sd(newd[,c],na.rm=TRUE)
  })
F<-matrix(rep(f,length(f)),nrow=length(f),ncol=length(f))   #diag(F)
F<-sapply(1:ncol(F),function(c){F[,c]<-F[,c]/F[c,c]})   #imagenan(F)  imagenan(Td) plot(F[,1])  plot(f)
testd<- -F%*%t(newd)/ncol(F)   # imagenan(t(testd));  im
sdc1<-sapply(1:ncol(newd),function(c){
  sd(newd[,c]-t(testd)[,c],na.rm=TRUE)/sd(newd[,c],na.rm=TRUE)
})
# plot(sdc,main="sign is 1"); plot(sdc1,main="sign is -1")
if(length(sdc[!is.na(sdc)])>0){
if(min(sdc,na.rm=TRUE)<min(sdc1,na.rm=TRUE)){
  sgn<-1
  xref<-which(sdc==min(sdc))
  } else {
    sgn<- (-1)
    xref<-which(sdc1==min(sdc1))

  }

cat("\nsign ",sgn," ref ",xref,"\n")
#   #imagenan(orig); imagenan(newd)
# f<- sgn*f     #eigenvectors.t %*% newdata.t   imagenan(newd)  xref<-3
# xref<-ncol(orig);
cat("\nSIGN ",sgn)
A<-make_A(xref,f)   #imagenan(A);imagenan(B)

# Tdata<-make_data_fromE(orig=newd,A=A,zero=0, main=main,maxA=50)
# Tx<-Tdata$Tx+mcm   #imagenan(Tdata$Tx)
# imagenan(Tx,main="Data From Eigenvector+mcm"); cat("\nResid errorTransformEigen-orig ",sd(Tx-(orig),na.rm=TRUE),"\n") #

B<-make_B(xref=xref,orig=sgn*orig,A=A) #plot(B[,ncol(B)]) ; plot(A[,ncol(A)])   : imagenan(orig)

Tdata<-make_data_fromE(orig=(orig),A=(sgn*A),B=B,zero=0, main=main,maxA=50)
Tx<-Tdata$Tx
} else{
  Tdata<-Tx<-NA
}
#imagenan(Tx,main="Data From Eigenvector");# cat("\nResid error Transform(Eigen)-orig ",sd(Tx-(orig)),"\n") #

# ones<-matrix(rep(1,ncol(orig)*nrow(orig)),nrow=ncol(A),ncol=nrow(orig))
# #Tdata<- A[:1:13] %*%t(orig)[1:13,] /ncol(A)+ B%*%ones/ncol(B)  # plot(sapply(1:nrow(newd),function(t)sum(A[15,1:10]*t(newd)[1:10,t])))
# Tdata<- sgn*A[,1:13] %*%t(newd)[1:13,] /ncol(A)+ B%*%ones/ncol(B)    #mean(B%*%ones/ncol(B))   imagenan(sgn*A %*%t(newd) /ncol(A))   plot(A[3,])
# #imagenan(matrix(Tdata,ncol=ncol(A),nrow=nrow(newd)))
# #plot(A[1:21,]%*%t(newd)[,200])     # ncol(t(newd))   plot(t(newd)[20,]) ncol(t(newd))

                                       #imagenan(t(e1)) imagenan(t(e2),zlim=c(-0.1,0.1)) ; imagenan(Tdata,zlim=c(-0.1,0.1))
#imagenan((orig),main="Original")  # imagenan(orig) ; imagenan(orig,zlim=c(-0.1,0.1))
 return(Tdata)
}#makedata from eigenvector and time average

# f<-eigenvectors.t[1,]
# A<-make_A(xref,f)
# B<-make_B(xref,orig,A)
#
# Tsimple<-make_data_fromE(orig=Td$smat,A=Td$E.s,B=Td$E.b,zero=Td$l.s.zero, main=" ")   #make data from simple slope intercept
# #Tdata<-make_datafrom_eigen(eigenvect,A,B,xref,orig)                  #makedata from eigenvector and time average
#
# Teigen<-make_data_frompca(orig=Td$smat,main=" ")     #make data from simple T and 1st 3 eigenvectors
# calc_pca(x=Td$smat,main=" ")


plot_hist<-function(Tdave,refer=NULL,main=" ",slim=NULL,blim=NULL){

  if(!is.null(refer)){

    cat("\n SLOPE summary for best" ,main," \n")
    si<-summary(c(Tdave$E.s[,refer]),na.rm=TRUE)
    print(si)
    #if(is.null(slim))slim<-c(si["Median"]-3*abs(si["Median"]-si["1st Qu."]),si["Median"]+3*abs(si["Median"]-si["3rd Qu."]))

    if(is.null(slim)){slim<-c(mean(c(Tdave$E.s),na.rm = TRUE)-2*sd(c(Tdave$E.s),na.rm = TRUE),
                              mean(c(Tdave$E.s),na.rm = TRUE)+2*sd(c(Tdave$E.s),na.rm = TRUE))}
    hist(Tdave$E.s[,refer],breaks=30,xlim=slim,main="")
    title(main=paste("\nEquitable Matrix SLOPES for",main ),cex.main=0.7)
    lines(rep(si["Median"],1000),0:999,lwd=4)

    lines(rep(si["1st Qu."],1000),0:999,lty=2,lwd=2)
    lines(rep(si["3rd Qu."],1000),0:999,lty=3,lwd=2)
    lines(rep(1,1001),seq(0,0.5,by=(0.5/1000)),lty=1,lwd=8)

    legend<-c(paste("Median",round(si["Median"],digits=2)),paste("1st Qu.",round(si["1st Qu."],digits=2)),
              paste("3rd Qu.",round(si["3rd Qu."],digits=2)),"Slope=1")
    lwd<-c(4,2,2,8)
    lty<-c(1,2,3,1)
    legend('topleft',inset=.02,legend=legend,
           lwd=lwd,lty=lty,bg='white',cex=0.75)



    cat("\n INTERCEPT summary for best" ,main," \n")
    bi<-summary(c(Tdave$E.b[,refer]),na.rm=TRUE)
    print(bi)
    #if(is.null(blim))blim<-c(bi["Median"]-3*abs(bi["Median"]-si["1st Qu."]),bi["Median"]+3*abs(bi["Median"]-bi["3rd Qu."]))
    if(is.null(blim)){blim<-c(mean(c(Tdave$E.b),na.rm = TRUE)-4*sd(c(Tdave$E.b),na.rm = TRUE),
                              mean(c(Tdave$E.b),na.rm = TRUE)+4*sd(c(Tdave$E.b),na.rm = TRUE))}
    if(length(which(!is.na(Tdave$E.b[,refer])))>0){
      hist(Tdave$E.b[,refer],breaks=50,xlim=blim,main="")
      title(main=paste("\nEquitable Matrix INTERCEPTS for",main ),cex.main=0.7)
      lines(rep(bi["Median"],1000),0:999,lwd=4)

      lines(rep(bi["1st Qu."],1000),0:999,lty=2,lwd=2)
      lines(rep(bi["3rd Qu."],1000),0:999,lty=3,lwd=2)
      lines(rep(0,1001),seq(0,0.5,by=(0.5/1000)),lty=4,lwd=8,type="p",pch=19)
      legend<-c(paste("Median",round(bi["Median"],digits=0)),paste("1st Qu.",round(bi["1st Qu."],digits=0)),
                paste("3rd Qu.",round(bi["3rd Qu."],digits=0)),"Intercept=0")
      lwd<-c(4,2,2,8)
      lty<-c(1,2,3,1)
      legend('topleft',inset=.02,legend=legend,
             lwd=lwd,lty=lty,bg='white',cex=0.75)
    } else {cat("\n Intercept column is completely NAs\n")}
  } else {

    cat("\n SLOPE summary  \n")
    si<-summary(c(Tdave$E.s),na.rm=TRUE)
    print(si)
    #if(is.null(slim))slim<-c(si["Median"]-3*abs(si["Median"]-si["1st Qu."]),si["Median"]+3*abs(si["Median"]-si["3rd Qu."]))
    if(is.null(slim)){
      #slim<-c(min(c(Tdave$E.s),na.rm=TRUE)-3*sd(c(Tdave$E.s),na.rm=TRUE),max(c(Tdave$E.s)+3*sd(c(Tdave$E.s),na.rm=TRUE),na.rm=TRUE))
      slim<- c(mean(c(Tdave$E.s),na.rm = TRUE)-2*sd(c(Tdave$E.s),na.rm = TRUE),
               mean(c(Tdave$E.s),na.rm = TRUE)+2*sd(c(Tdave$E.s),na.rm = TRUE))
      hist(Tdave$E.s,xlim=slim,main="")    #
    } else {
      hist(Tdave$E.s,breaks=30,xlim=slim,main="")
    }
    title(main=paste("\nEquitable Matrix SLOPES for",main ),cex.main=0.7)
    lines(rep(si["Median"],1000),0:999,lwd=4)

    lines(rep(si["1st Qu."],1000),0:999,lty=2,lwd=2)
    lines(rep(si["3rd Qu."],1000),0:999,lty=3,lwd=2)
    lines(rep(1,1001),seq(0,0.5,by=(0.5/1000)),lty=1,lwd=8,type="p",pch=19)
    legend<-c(paste("Median",round(si["Median"],digits=2)),paste("1st Qu.",round(si["1st Qu."],digits=2)),
              paste("3rd Qu.",round(si["3rd Qu."],digits=2)),"Slope=1")
    lwd<-c(4,2,2,8)
    lty<-c(1,2,3,1)
    legend('topleft',inset=.02,legend=legend,
           lwd=lwd,lty=lty,bg='white',cex=0.75)


    cat("\n SLOPE summary (Exclude slopes that cannot be distinguished from 1) \n")
    nsig=2
    sss<-Tdave$E.s
    sse<-nsig*Tdave$E.sd1/sqrt(Tdave$E.sN)
    sss[abs(sss-1)-sse<0]<-NA
    si<-summary(c(sss),na.rm=TRUE)
    print(si)
    #if(is.null(slim))slim<-c(si["Median"]-3*abs(si["Median"]-si["1st Qu."]),si["Median"]+3*abs(si["Median"]-si["3rd Qu."]))
    if(is.null(slim)){c(mean(c(Tdave$E.s),na.rm = TRUE)-2*sd(c(Tdave$E.s),na.rm = TRUE),
                        mean(c(Tdave$E.s),na.rm = TRUE)+2*sd(c(Tdave$E.s),na.rm = TRUE))}
    hist(sss,breaks=30,xlim=slim,main="")
    title(main=paste("\nSLOPES (Exclude slopes not different from1)",main ),cex.main=0.7)
    lines(rep(1.1,1000),0:999,lty=1,lwd=3)
    lines(rep(1/1.1,1000),0:999,lty=2,lwd=2)
    lines(rep(1,1001),seq(0,0.5,by=(0.5/1000)),lty=1,lwd=8,type="p",pch=19)
    legend<-c(paste("Slope=1.1"),paste("Slope=",round(1/1.1,digits=2))
              ,"Slope=1")
    lwd<-c(3,2,8)
    lty<-c(1,2,1)
    legend('topleft',inset=.02,legend=legend,
           lwd=lwd,lty=lty,bg='white',cex=0.75)


    cat("\n INTERCEPT summary  \n")
    bi<-summary(c(Tdave$E.b),na.rm=TRUE)
    print(bi)
    #if(is.null(blim))blim<-c(bi["Median"]-3*abs(bi["Median"]-bi["1st Qu."]),bi["Median"]+3*abs(bi["Median"]-bi["3rd Qu."]))
    if(is.null(blim)){
      #blim<-c(min(c(Tdave$E.b),na.rm=TRUE)-3*sd(c(Tdave$E.b),na.rm=TRUE),max(c(Tdave$E.b)+3*sd(c(Tdave$E.b),na.rm=TRUE),na.rm=TRUE))
      blim<- c(mean(c(Tdave$E.b),na.rm = TRUE)-4*sd(c(Tdave$E.b),na.rm = TRUE),mean(c(Tdave$E.b),na.rm = TRUE)+4*sd(c(Tdave$E.b),na.rm = TRUE))
      hist(Tdave$E.b,xlim=blim,main="")
    } else {
      hist(Tdave$E.b,breaks=50,xlim=blim,main="")
    }

    title(main=paste("\nEquitable Matrix INTERCEPTS for",main ),cex.main=0.7)
    lines(rep(bi["Median"],1000),0:999,lwd=4)

    lines(rep(bi["1st Qu."],1000),0:999,lty=2,lwd=2)
    lines(rep(bi["3rd Qu."],1000),0:999,lty=3,lwd=2)
    lines(rep(0,1001),seq(0,0.5,by=(0.5/1000)),lty=4,lwd=8,type="p",pch=19)
    legend<-c(paste("Median",round(bi["Median"],digits=0)),paste("1st Qu.",round(bi["1st Qu."],digits=0)),
              paste("3rd Qu.",round(bi["3rd Qu."],digits=0)),"Intercept=0")
    lwd<-c(4,2,2,8)
    lty<-c(1,2,3,1)
    legend('topleft',inset=.02,legend=legend,
           lwd=lwd,lty=lty,bg='white',cex=0.75)
  }
}

#a_b_bagplot(slope=Tdall$E.s,intercept=Tdall$E.b,community.f=sectors.f,zero=Tdall$l.s.zero,refindex="Row_Ave",xlim=xlim,ylim=ylim)

add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2,
        function(x)
          rgb(x[1], x[2], x[3], alpha=alpha))
}


# Calculate metrics corresponding to the Standard Ellipse based on a
# covariance matrix https://rdrr.io/cran/SIBER/man/sigmaSEA.html
#
# This function takes a covariance 2x2 matrix Sigma and returns various
# metrics relating to the corresponding Standard Ellipse. The function is
# limited to the 2-dimensional case, as many of the ancillary summary
# statistics are not defined for higher dimensions (e.g. eccentricity).
#
# @section Note: This function is currently based on the eigenvalue and
#   eigenvector approach which is more flexible for higher dimensional problems
#   method for calculating the standard ellipse, and replaces the parametric
#   method used previously in siar and siber.
#
# @param sigma a 2x2 covariance ellipse.
#
# @return A list comprising the following metrics for summarising the Standard
# Ellipse
# #' \itemize{
#   \item {SEA}{ the Standard Ellipse Area (not sample size corrected)}
#    \item {eccentricity}{ a measure of the elongation of the ellipse.}
#  \item {a}{ the length of the semi-major axis}
#   \item {b}{ the length of the semi-minor axis}
# }
#
sigmaSEA <- function(sigma){

  eig <- eigen(sigma)

  a <- sqrt(eig$values[1])
  b <- sqrt(eig$values[2])

  # As of v2.0.4 I have replaced the asin() line with atan which
  # returns the angle of correct sign due to the inclusion of the quotient
  # of the vectors.
  theta <- atan(eig$vectors[2,1] / eig$vectors[1,1])
  thetadegree=theta*180/pi
  SEA <- pi*a*b


  out <- list()
  out$SEA <- pi*a*b
  out$eccentricity <- sqrt(1-((b^2)/(a^2)))
  out$a <- a
  out$b <- b
  out$theta <- theta
  out$thetadegree <- thetadegree
  return(out)
}

find_ellipse<-function(p,main="",plotf=TRUE,ylim=NULL,xlim=NULL,maxlev=3,yl="Y",xl="X",xc=0,yc=0){
  if(is.null(ylim)){
    ylim<-c(min(p[,2],na.rm = TRUE),max(p[,2],na.rm = TRUE))
  }
  if(is.null(xlim)){
    xlim<-c(min(p[,1],na.rm = TRUE),max(p[,1],na.rm = TRUE))
  }

  center <- apply(p, 2, mean)
  sigma <- cov(p)
  testsig<-min(c(length(unique(p[,1])) ,length(unique(p[,2]))),na.rm=TRUE )
  if(length(which(is.na(sigma)| is.nan(sigma)|is.infinite(sigma)))==0 && testsig>2){

  s_info<-sigmaSEA(sigma)
  Area<-pi*s_info$a*s_info$a
  xsc<-(ylim[2]-ylim[1])/(xlim[2]-xlim[1])
  pnew<-p; pnew[,1]<-xsc*pnew[,1]
  centernew <- apply(pnew, 2, mean)
  sigmanew <- cov(pnew)
  s_info_new<-sigmaSEA(sigmanew)    # length(which(is.na(sigma)| is.nan(sigma)|is.infinite(sigma))) sigma[1,1]<-NaN; sigma[2,2]<-NA
  Area_new<-pi*s_info_new$a*s_info_new$a
  if(plotf ){
    hist(p[,1],xlim=xlim,xlab=xl,main=paste("Ellipse:x axis",main=main),cex.main=0.8);
    hist(p[,2],xlim=ylim,xlab=yl,main=paste("Ellipse:y axis",main=main),cex.main=0.8)

    aplpack::bagplot(p,ylim=ylim,xlim=xlim,main=paste("Ellipse:",main), xlab=xl, ylab=yl,cex.main=0.8)
    lines(rep(xc,100),seq(ylim[1],ylim[2],by=(ylim[2]-ylim[1])/99))
    lines(seq(xlim[1],xlim[2],by=(xlim[2]-xlim[1])/99),rep(yc,100))
    #The ellipses are determined by the first and second moments of the data:
    sigma.inv = solve(sigma, matrix(c(1,0,0,1),2,2))
    n <- 100
    x <- (0:(n-1)) * ((xlim[2]-xlim[1])/(n-1))+xlim[1]
    y <- (0:(n-1)) * ((ylim[2]-ylim[1])/(n-1))+ylim[1]
    ellipse <- function(s,t) {u<-c(s,t)-center; u %*% sigma.inv %*% u / 2}
    #Compute the height function at this grid and plot it:
    z <- mapply(ellipse, as.vector(rep(x,n)), as.vector(outer(rep(0,n), y, `+`)))
    plot(p, pch=20, xlim=xlim, ylim=ylim, xlab=xl, ylab=yl,main=main,cex.main=0.8)
    lines(rep(xc,100),seq(ylim[1],ylim[2],by=(ylim[2]-ylim[1])/99))
    lines(seq(xlim[1],xlim[2],by=(xlim[2]-xlim[1])/99),rep(yc,100))
    colour<-c("red","green","blue","orange","cyan","yellow",rep("black",30))

    contour(x,y,matrix(z,n,n), levels=(1:maxlev), col = colour[1:maxlev], add=TRUE,lwd=3)
    legend("topleft",legend=c("Data",paste("Level",1:maxlev)),pch=c(20,rep(NA,maxlev)),lty=c(NA,rep(1,maxlev)),lwd=c(NA,rep(3,maxlev)),
           col=c("black",colour[1:maxlev]) )
    legend("bottomright",legend=c(paste("N=",nrow(p)),
                                  paste("Center",
                                  round(center[1],digits=2),",",round(center[2],digits=2)),
                                  paste("x Scale factor",round(xsc,digits=2)),
                                  paste("Eccentricity",round(s_info_new$eccentricity,digits=3)),
                                  paste("a-Axis",round(s_info_new$a,digits=2)),
                                  paste("b-Axis",round(s_info_new$b,digits=2)),
                                  paste("Area",round(Area_new,digits=2)),
                                  paste("Angle (deg)",round(s_info_new$thetadegree,digits=1))),cex=0.8)
  }
  cat("\nBasic info for ellipse run")
  cat(main)
  cat("\nCenter (Unscaled)",center)
  cat(main)
  cat("\nUnscaled Elliptical information")
  print(s_info)
  cat("\nX Scaled Elliptical information using xsc of ",xsc)
  print(s_info_new)
  } else{
    cat("\nEllipse: Sigma has invalid values")
    print(sigma)
    s_info<-s_info_new<-Area<-Area_new<-NA;
  }
  return(list=c(sigma=sigma,sigma=sigmanew,s_info=s_info,s_info_new=s_info_new,Area=Area,Area_new=Area_new))
}

biplotellipse<-function(allfacave_more=NULL,nellipse=NULL,typename=NULL,
                        treat_type=NULL,treat=NULL,lcorr=NULL,main="",corrname=NULL,namesprofile=NULL){
  #change all otc.willow  to  allfacave_more[,"OTC"]
  if(lcorr[4]=="OTC"){
    allfacave_more[which(allfacave_more[,"OTC"]=="OTC.Willow"),"OTC"]<-"OTC"
    allfacave_more[which(allfacave_more[,"OTC"]=="Control.Willow"),"OTC"]<-"Control"
  }

  if(is.null(treat_type)){treat_type<-"None";main<-paste(treat_type,main)}


  if(length(which(colnames(allfacave_more)=="Event"))!=0){
    ev<-unique(allfacave_more[,"Event"])
    evname<-"Event"
    } else {
      ev<-unique(allfacave_more[,"Event.MIDAll_Together"])
      evname<-"Event.MIDAll_Together"
    }
  if(!is.null(namesprofile)){
    ev1<-namesprofile
  } else ev1<-ev
  if(is.null(treat)){
  numev<-rep(0,length(ev1))   # k<-10
  for(k in 1:length(ev)) numev[which(ev1==ev[k])]<-length(which(allfacave_more[,evname]==ev[k]))
  names(numev)<-ev1
  barplot(numev,main=main,cex.main=0.8)
  cat("\n",main,"\n")
  print(numev)
  cat("\n Peak number of intersections",max(numev)," events at ",names(numev)[which(numev==max(numev))],"\n")
  } else {
    numev<-rep(NA,length(ev1))
    for(k in 1:length(ev)) numev[which(ev1==ev[k])]<-length(which(allfacave_more[,evname]==ev[k] & allfacave_more[,treat_type]==treat))
    names(numev)<-ev1
    barplot(numev,main=main,cex.main=0.8)
    cat("\n",main,"\n")
    print(numev)
    cat("\n Peak number of intersections",max(numev)," events at ",names(numev)[which(numev==max(numev))],"\n")

  }
  #corrname-variable to correlate with eg. "Year" if NULL then use all (N-1) of the "treat_types" in lcorr
  #                                                                        except for the terean=menrt being applied
  #extract data for treat_type= name of treatment  "SnowTreat"  "OTC"  if NULL then use all the data
  #treat is actual treatment   rem add control
  #if(is.null(corrname)) corrname<-which(lcorr!=treat_type)
  if(!is.null(treat)){
    if(is.null(corrname)) correlate_with<-lcorr[which(lcorr[1:(length(lcorr)-1)]!=treat_type)]else correlate_with<-c(corrname)
    allfacave_treat<-allfacave_more[which(allfacave_more[,treat_type]==treat),]
  }  else {
    if(is.null(corrname)) correlate_with<-lcorr[which(lcorr[1:(length(lcorr)-1)]!=treat_type)]else correlate_with<-c(corrname)


    allfacave_treat<-allfacave_more
  }
  if(is.null(treat_type))treat_type<-"No"
  for(corrnm in correlate_with){#corrnm<-"Year"  corrnm<-"Site"


    if(length(which(complete.cases(allfacave_treat[,c(corrnm,nellipse)])))>=2){

      newd<-as.data.frame(NA,nrow=nrow(allfacave_treat))
      # if(corrnm!="OTC") {
      # for(nk in 1:length(lcorr))newd<-cbind(newd,as.numeric(allfacave_treat[,lcorr[nk]]))
      # } else{
      #   for(nk in 1:length(lcorr))newd<-cbind(newd,allfacave_treat[,lcorr[nk]])
      # }
      for(nk in 1:length(lcorr)){
        if(lcorr[nk]=="OTC" || lcorr[nk]=="Site"){
          newd<-cbind(newd,allfacave_treat[,lcorr[nk]])   #colnames(allfacave_treat)allfacave_treat[,"Year"]
        } else{

          newd<-cbind(newd,as.numeric(allfacave_treat[,lcorr[nk]]))
      }

      }
      newd<-newd[,-1]
      for(nk in 1:length(lcorr))colnames(newd)[nk]<-lcorr[nk]    #ncol(newd) ncol(allfacave_treat)
      rownames(newd)<-1:nrow(newd)
      cat("\n N= ",nrow(newd)," for treat=",treat,"\n")
      print(newd)
      if(nellipse=="a"||
         nellipse=="b"|| nellipse=="Area"||
         nellipse=="Area"|| nellipse=="eccentricity"|| nellipse=="angle") yll<-paste("Ellipse",nellipse) else yll<-nellipse
      #ydata<-as.numeric(as.character(newd[,nellipse]))
      ylim=c(min(newd[,nellipse],na.rm=TRUE),max(newd[,nellipse],na.rm=TRUE))
  if(corrnm!="OTC" && corrnm!="Site") {

    plot(newd[,corrnm],newd[,nellipse],pch=15,cex=2,cex.lab=1.5,
           main=main,
           xlab=corrnm, ylab=yll,ylim=ylim)
    cat("\n corrnm= ",corrnm,"\n")
    fit<-lm(newd[,nellipse]~newd[,corrnm], na.action=na.exclude )
    fit_coef<-coef(summary(fit))
    if(dim(fit_coef)[1]==2) {
      s=fit_coef[2,"Estimate"]
      sse=fit_coef[2,"Std. Error"]
      b=fit_coef["(Intercept)","Estimate"]
      bse=fit_coef["(Intercept)","Std. Error"]
      r2=summary(fit)$r.squared
      N=nrow(newd)
      pslope=fit_coef[2,"Pr(>|t|)"]
      legend("topright",
             legend = c(paste0("Slope= ",round(s,digits=2) ),paste0("(95% ",round(2*sse,digits=2),") N=",N ),
                        paste0("Inter.= ",round(b,digits=1)),paste0("(95% ",round(2*bse,digits=1),")"),
                        paste("p-Value=",round(pslope,digits=5) ) ))
      abline(fit,lwd=2,ylim=ylim)
       cat(main)
      cat("\n corrnm= ",corrnm,"\n")
      print(summary(fit) )
    }
  }    else{
   if( treat_type=="None"){
     if(corrnm!="Site"){
     cat("\n corrnm= ",corrnm,"\n")
    boxplot(as.numeric(as.character(newd[,nellipse]))~newd[,corrnm]*allfacave_more[,"Site"],
            pch=15,cex=2,cex.lab=1.5,col=c("blue","red"),
            main=main,
            xlab=corrnm, ylab=yll,ylim=ylim)


     fit<-lm(newd[,nellipse]~newd[,corrnm], na.action=na.exclude )
     cat(main)
     print(summary(fit))
     cat("\nCorrnm = ",corrnm,"\n")

     cat("\n using site as random effect\n")
     print(summary(lmer(newd[,nellipse] ~ newd[,corrnm] +( newd[,corrnm] |allfacave_more[,"Site"]) ,  na.action=na.exclude)))
     }


    }
    boxplot(as.numeric(as.character(newd[,nellipse]))~newd[,corrnm],pch=15,cex=2,cex.lab=1.5,
         main=main,col=c("grey"),
         xlab=corrnm, ylab=yll,ylim=ylim)
    fit<-lm(newd[,nellipse]~newd[,corrnm], na.action=na.exclude )
    fit_coef<-coef(summary(fit))
    if(dim(fit_coef)[1]>=2) {

      legend("topright",
             legend = c( paste("N=",length(which(!is.na(newd[,nellipse])) )),
                        paste("p-Value=",round(anova(fit)$'Pr(>F)'[1],digits=5) ) ))

      cat(main)
      cat("\n corrnm= ",corrnm,"\n")
      print(summary(fit) )    #summary(fit)$cov.unscaled   str(summary(fit)) summary(fit)$coefficients anova(fit)$'Pr(>F)'[1]

           }


    }
  }
  }

      l1<-1;l2<-4
      if(length(which(complete.cases(newd[,lcorr[l1]] , newd[,lcorr[l2]])))>=2){
      cat(main)
      cat(" correlate ",nellipse," with indices l1=",lcorr[l1],l1," and l2=",lcorr[l2],l2  ,"\n")
      print(summary(lm(newd[,nellipse] ~ newd[,lcorr[l1]] + newd[,lcorr[l2]] , data = newd, na.action=na.exclude)))
      cat(main)
      cat(" correlate ",nellipse," with indices l1=",lcorr[l1],l1," and l2=",lcorr[l2],l2  ,"\n")
      print(summary(lm(newd[,nellipse] ~ newd[,lcorr[l1]] * newd[,lcorr[l2]] , data = newd, na.action=na.exclude)))
      }

      l1<-2;l2<-4
      if(length(which(complete.cases(newd[,lcorr[l1]] , newd[,lcorr[l2]])))>=2){
      cat(main)
      cat("correlate ",nellipse," with indices l1=",lcorr[l1],l1," and l2=",lcorr[l2],l2  ,"\n")
      print(summary(lm(newd[,nellipse] ~ newd[,lcorr[l1]] + newd[,lcorr[l2]] , data = newd, na.action=na.exclude)))
      cat(main)
      cat(" correlate ",nellipse," with indices l1=",lcorr[l1],l1," and l2=",lcorr[l2],l2  ,"\n")
      print(summary(lm(newd[,nellipse] ~ newd[,lcorr[l1]] * newd[,lcorr[l2]] , data = newd, na.action=na.exclude)))
      }

      l1<-3;l2<-4
      if(length(which(complete.cases(newd[,lcorr[l1]] , newd[,lcorr[l2]])))>=2){
      cat(main)
      cat(" correlate ",nellipse," with indices l1=",lcorr[l1],l1," and l2=",lcorr[l2],l2  ,"\n")
      print(summary(lm(newd[,nellipse] ~ newd[,lcorr[l1]] + newd[,lcorr[l2]] , data = newd, na.action=na.exclude)))
      cat(main)
      cat(" correlate ",nellipse," with indices l1=",lcorr[l1],l1," and l2=",lcorr[l2],l2  ,"\n")
      print(summary(lm(newd[,nellipse] ~ newd[,lcorr[l1]] * newd[,lcorr[l2]] , data = newd, na.action=na.exclude)))
      }

      l1<-1;l2<-2
      if(length(which(complete.cases(newd[,lcorr[l1]] , newd[,lcorr[l2]])))>=2){
      cat(main)
      cat(" correlate ",nellipse," with indices l1=",lcorr[l1],l1," and l2=",lcorr[l2],l2  ,"\n")
      print(summary(lm(newd[,nellipse] ~ newd[,lcorr[l1]] + newd[,lcorr[l2]] , data = newd, na.action=na.exclude)))
      cat(main)
      cat(" correlate ",nellipse," with indices l1=",lcorr[l1],l1," and l2=",lcorr[l2],l2  ,"\n")
      print(summary(lm(newd[,nellipse] ~ newd[,lcorr[l1]] * newd[,lcorr[l2]] , data = newd, na.action=na.exclude)))
      }

      l1<-1;l2<-3
      if(length(which(complete.cases(newd[,lcorr[l1]] , newd[,lcorr[l2]])))>=2){
      cat(main)
      cat(" correlate ",nellipse," with indices l1=",lcorr[l1],l1," and l2=",lcorr[l2],l2  ,"\n")
      print(summary(lm(newd[,nellipse] ~ newd[,lcorr[l1]] + newd[,lcorr[l2]] , data = newd, na.action=na.exclude)))
      cat(main)
      cat(" correlate ",nellipse," with indices l1=",lcorr[l1],l1," and l2=",lcorr[l2],l2  ,"\n")
      print(summary(lm(newd[,nellipse] ~ newd[,lcorr[l1]] * newd[,lcorr[l2]] , data = newd, na.action=na.exclude)))
      }

      l1<-2;l2<-3
      if(length(which(complete.cases(newd[,lcorr[l1]] , newd[,lcorr[l2]])))>=2){
      cat(main)
      cat(" correlate ",nellipse," with indices l1=",lcorr[l1],l1," and l2=",lcorr[l2],l2  ,"\n")
      print(summary(lm(newd[,nellipse] ~ newd[,lcorr[l1]] + newd[,lcorr[l2]] , data = newd, na.action=na.exclude)))
      cat(main)
      cat(" correlate ",nellipse," with indices l1=",lcorr[l1],l1," and l2=",lcorr[l2],l2  ,"\n")
      print(summary(lm(newd[,nellipse] ~ newd[,lcorr[l1]] * newd[,lcorr[l2]] , data = newd, na.action=na.exclude)))
      }

      l1<-1;l2<-2;l3=4
      if(length(which(complete.cases(newd[,lcorr[l1]] , newd[,lcorr[l2]], newd[,lcorr[l3]])))>=2){
      cat(main)
      cat(" correlate ",nellipse," with indices l1=",lcorr[l1],l1,"l2=",lcorr[l2]," and l3=",lcorr[l3],l3  ,"\n")
      print(summary(lm(newd[,nellipse] ~ newd[,lcorr[l1]] + newd[,lcorr[l2]] + newd[,lcorr[l3]], data = newd, na.action=na.exclude)))
      }

      l1<-2;l2<-3;l3=4
      if(length(which(complete.cases(newd[,lcorr[l1]] , newd[,lcorr[l2]], newd[,lcorr[l3]])))>=2){
      cat(main)
      cat(" correlate ",nellipse," with indices l1=",lcorr[l1],l1,"l2=",lcorr[l2]," and l3=",lcorr[l3],l3  ,"\n")
      print(summary(lm(newd[,nellipse] ~ newd[,lcorr[l1]] + newd[,lcorr[l2]] + newd[,lcorr[l3]], data = newd, na.action=na.exclude)))
      }


  if(length(which(complete.cases(allfacave_treat[,lcorr])))>=6 && lcorr[4]!="OTC"&& lcorr[4]!="Site"){

    tr<-rep(NA,(length(lcorr)-2))   #tr[3]<-FALSE    test first N-2 lcorr values for some variation
    for (facter in 1:(length(lcorr)-2))tr[facter]<-sd(allfacave_more[,facter],na.rm=TRUE)>0.02
    #if(all(tr, na.rm = TRUE)) print("true") else print("FALSE")
    #sd(allfacave_more[,"June.Temp"],na.rm=TRUE)>0.02 && sd(allfacave_more[,"August.Temp"],na.rm=TRUE)
    if(all(tr, na.rm = TRUE)){
      # data.frame(col = rep(colnames(newd1), each = nrow(newd1)),
      #            row = rep(rownames(newd1), ncol(newd1)),
      #            value = as.vector(newd1))
      fit<-princomp(formula = ~., data =newd  , cor = TRUE,scores =TRUE, na.action=na.exclude)
      cat(main)
      print(summary(fit) )# print variance accounted for
      loadings(fit) # pc loadings
      #plot(fit$sdev,ylim=c(0,5)) # scree plot
      plot(fit,ylim=c(0,fit$sdev[1]^2),main=paste("PCA:", main),cex.main=0.8) # scree plot
      # biplot(fit,main=paste("PCA:", main,"\n"),var.axes=TRUE,lwd=4)
      biplot(fit,main=paste("PCA:", main,"\n"),var.axes=TRUE,
             lwd=4,arrow.len=0.2,expand=0.8,xlabs=rep(".", nrow(newd)),col="black",cex.main=0.8,
             cex.lab=1.5, cex.axis=1.5,  cex.sub=1.5 ,cex=1.5) #col=c("white","black"),
    } else cat("\n PCA 3: little change in June or August  T\n")
  }

  ## end Biplotellipse
}


#' Creates bagplots of equitable intercepts versus 1-slope
#'
#' Bagplots are made by varying the zero of ther data set to various values the variable t(dependent on options).
#' If community.f has some nonnumeric trait then bagplots are also created for each category of the trait.
#' Plots are made based on the reference column chosen.
#'
#' @param Td  Transform info based on transformE
#' @param community.f nonnumeric character traits of the individuals in x: default NULL
#' @param refindex intercept values based on this refences x location or individual: Default "Row_Ave"
#' @param xlim vector of c(lowest range,highest range) for 1-slope values default NULL function defines them
#' @param ylim vector of c(lowest range,highest range) for intercept values default NULL function defines them
#' @param rownum vector of points on t to use as zeroes default NULL (about 10 t values chosen)
#' @param facname name of factor characteristic
#' @param fall use all values of t Default=FALSE
#' @param main text heading for plot
#'
#' @return index of best intersection of plotted sequences
#'
#' @examples
#'
#' d<-eg8(rmult=4,cmult=4,mf=10,mg= 0,mu=10,sdf=0.75,sdg=1,sdu=0.2)
#' Td<-transformE(d=d)
#' aa<-a_b_bagplot(Td=Td,xlim=c(-0.3,0.3),ylim=c(-4,4))
#' aa<-a_b_bagplot(Td=Td,xlim=c(-0.3,0.3),ylim=c(-5,5),rownum=seq(1,ncol(Td$smat),by=2))
#' aa<-a_b_bagplot(Td=Td,xlim=c(-0.2,0.2),ylim=c(-5,5),fall=TRUE)
#'
#' @export
a_b_bagplot<-function(community.f=NULL,Td,
                      refindex="Row_Ave",xlim=NULL ,ylim=NULL,main=" ",rownum=NULL,facname=NULL,fall=FALSE){  #slope,intercept,
  #lotscol<-colors()[c(24,94,26,130,121,96,97,49,47,417,256,8,33,90,142,144,653)]
  if(ncol(Td$E.b)<60) {quant1<-0.33;quant2<-0.67 }
  if(ncol(Td$E.b)>60 && nrow(Td$E.b)<=150 ) {quant1<-0.1;quant2<-0.9 }
  if(ncol(Td$E.b)>150 ) {quant1<-0.05;quant2<-0.95 }   # quant1<-0.0;quant2<-1
  bestintersectname<-NULL

  minpoints<-4 # more than 4 points needed to find linear fits to best event
  lotscol<-colors()[c(24,94,26,124,633,450,453,11,68,254,257,51,630,76,142,150,653)]
  translevel<-0.35
  transgrey<-add.alpha("grey",translevel)
  lotscol1<-add.alpha(lotscol,translevel)
  # plot(1:length(lotscol),cex=3,col=lotscol,pch=15)
  # plot(1:length(lotscol),cex=3,col=vcolours,pch=15)

  if(!is.numeric(refindex)){
    refindex<-which(colnames(Td$smat)==refindex)
  }
  op =
    par(mfrow =  c(1,1), mar = c(8,4.5, 4.5,4))
  if(is.null(rownum)){ #zero not set so run through all values for rows of Row_Ave  column
    if(nrow(Td$ET.x)>20 && !fall)rinc<-ceiling((nrow(Td$ET.x))/(10)) else rinc=1
    ir<-seq(1,nrow(Td$ET.x),by=rinc)
    if(ir[length(ir)]!=nrow(Td$ET.x))ir<-c(ir,nrow(Td$ET.x))
    zerolist<-Td$ET.x[ir,refindex]
    vcolours<-lotscol1[1:length(ir)]
    colourpt<-lotscol[1:length(ir)]
  } else {
    if(is.numeric(rownum))ir<-rownum else ir<-which(rownames(Td$ET.x)==rownum)
       zerolist<-Td$ET.x[rownum,refindex]
       vcolours<-"blue"
       colourpt<-"blue"
  }
  cat("\nir is ",ir,"\nrownames are ",rownames(Td$ET.x)[ir],"\n")


  #run each factor/community for different zeroes but
  if(is.null(xlim)){
    ff<-quantile(c(1-Td$E.s[,refindex]),probs=c(0.0,1.00), na.rm = TRUE)  #imagenan(Td$E.s)   quantile(c(1-Td$E.s),probs=c(0.0,0.9), na.rm = TRUE)
    slim<-c(ff[1],ff[2]+(ff[2]-ff[1])/5)
  } else{
    slim<-xlim
  }
  if(is.null(ylim)){
    # ff<-quantile(c(Td$E.b[,refindex]),probs=c(quant1,quant2), na.rm = TRUE)
    # blim<-c(ff[1],ff[2])
    # ff<-quantile(c(Td$E.b[,refindex]),probs=c(quant1,quant2), na.rm = TRUE)
     ff<-quantile(c(Td$ET.x[,refindex]),probs=c(0,1), na.rm = TRUE)
    half<-(ff[2]-ff[1])/2
    blim<-c(-half,+half)
  } else{
    blim<-ylim
  }

    #   slim<-c(-0.5,0.5);
    # blim<-c(-20,20)   # same full scale for all years unless changed
  if(!is.null(community.f)){
    levc<-length(levels(community.f))
    if(levc>1){
    cat("\nmain factor\n")
    print(summary(community.f))

    slope<-Td$E.s[ ,refindex]   #colnames(slope)
    bcol<-lotscol[4:(length(levels(community.f))+3)]
    boxplot(slope ~community.f, na.action=na.exclude,col=bcol,lwd=2,notch=FALSE,
            main=paste("zero=",round(Td$l.s.zero,digits=1),"\n",facname, main),cex=1.0,cex.axis=0.7,ylab="Slope",cex.main=0.8,
            xlab=paste(facname),cex.lab=1.2)
    legend("topright",legend=levels(community.f),fill=bcol)
    cat("\n\n\nSlope Linear model ", " with ",levels(community.f),"\n\n")
    print(summary(lm(slope ~  community.f   ,  na.action=na.exclude)))

    #now run intercept for different zeros
    for( irow in ir){
      zero<-Td$ET.x[irow,refindex]

      zname<-rownames(Td$ET.x)[irow]
      # cat("\n Start",zero,zname)
      newb<-reviseb_witherror(rownum=irow,Td=Td,ref=refindex)   # revise the zero based on zero=x[rownum,ref]  imagenan(newb$smat)
      intercept<-newb$E.b[ ,refindex]
      # plot(intercept,main=zname)
      boxplot(intercept ~community.f, na.action=na.exclude,col=bcol,lwd=2,notch=FALSE,
              main=paste("zero=",round(zero,digits=1),zname,"\n",facname, main),cex=1.0,cex.axis=0.7,ylab="Intercept",cex.main=0.8,
              xlab=paste(facname),cex.lab=1.2)
      legend("topright",legend=levels(community.f),fill=bcol)
      cat("\n\n\nIntercept Linear model ", " with ",levels(community.f)," Event ",zname,"\n\n")
      print(summary(lm(intercept ~  community.f  ,  na.action=na.exclude)))

    }
   # return()
    # is set up for runs 150 to 241 but for long term data for Salix
    # for(i in 1:nrow(smat))
    # newb<-reviseb_witherror(rownum=irow,Td=Td,ref=refindex)   # revise the zero based on zero=x[rownum,ref]  imagenan(newb$smat)
    # slope<-newb$E.s   #colnames(slope)
    # print(newb$l.s.zero)
    # #zero<-newb$l.s.zero
    # intercept<-newb$E.b
    # par(mfrow=c(1,1))
    # x <- 1-slope[ which(community.f==levels(community.f)[j]),refindex]
    # y<-intercept[ which(community.f==levels(community.f)[j]),refindex]
    # dat<-cbind(x,y)
    maxsall<-maxball<-(-10^10); minsall<-minball<-10^10
    for(j in 1:length(levels(community.f))) {
      #first put all one one plot
     #colourpt=rainbow(length(ir))
      #vcolours<-colourpt<-lotscol[1:length(ir)]
      #colourpt2=rainbow(length(levels(f2)))

      #vcolours<-add.alpha(colourpt,translevel)
      #vcolours2<-add.alpha(colourpt2,translevel)
      transgrey<-add.alpha("grey",translevel)
      cat("\nStarting Community ",levels(community.f)[j]," ir = ",ir,"\n")

      pinter<-pslope<-NULL
      aslope<-NULL
      a<-NULL;c<-NULL;r2<-NULL;aerror<-NULL;cerror<-NULL;z<-NULL; N<-NULL;p<-NULL
      maxs<-maxb<-(-10^10); mins<-minb<-10^10

      for( irow in ir){
        zero<-Td$ET.x[irow,refindex]
       # cat("\n Start",zero)
        zname<-rownames(Td$ET.x)[irow]


        newb<-reviseb_witherror(rownum=irow,Td=Td,ref=refindex)   # revise the zero based on zero=x[rownum,ref]  imagenan(newb$smat)
        slope<-newb$E.s   #colnames(slope)
       # print(newb$l.s.zero)
        #zero<-newb$l.s.zero
        intercept<-newb$E.b

      #subset and use the site data
      #x <- smat[i, which(orig[,indexcol]==levels(community.f)[j])]
      x <- 1-slope[ which(community.f==levels(community.f)[j]),refindex]
      y<-intercept[ which(community.f==levels(community.f)[j]),refindex]
      #check that some x are different
      ncomp<-length(which(complete.cases(y,x)))    # sd(x,na.rm=TRUE)
      if(ncomp>minpoints && sd(x,na.rm=TRUE)>10^-9  && sd(y,na.rm=TRUE)>10^-9 ){
        fit<-lm(y~x, na.action=na.exclude )
        fit_coef<-coef(summary(fit))
        a<-c(a,fit_coef[2,"Estimate"])  #  fit_coef["x","Estimate"]
        aerror<-c(aerror,sse=fit_coef[2,"Std. Error"])
        c<-c(c,fit_coef["(Intercept)","Estimate"])
        cerror<-c(cerror,fit_coef["(Intercept)","Std. Error"])
        r2<-c(r2,summary(fit)$r.squared)
        N<-c(N,ncomp)
        p<-c(p,fit_coef["(Intercept)","Pr(>|t|)"])
        z<-c(z,zero)

        pinter<-c(pinter,fit_coef["(Intercept)","Pr(>|t|)"])
        names(pinter)[length(pinter)]<-zname
        pslope<-c(pslope,fit_coef["x","Pr(>|t|)"])
        names(pslope)[length(pslope)]<-zname
        cat("\n  ", " p(slope differs from 0) ",fit_coef["x","Pr(>|t|)"])
        aslope<-c(aslope,fit_coef[2,"Estimate"])
        names(aslope)[length(aslope)]<-zname

      } else {a<-c(a,NA);c<-c(c,NA);r2<-c(r2,NA);aerror<-c(aerror,NA);cerror<-c(cerror,NA);z<-c(z,NA)}



      maxsc<-max(x,na.rm=TRUE)
      maxbc<-max(y,na.rm=TRUE)
      minsc<-min(x,na.rm=TRUE)
      minbc<-min(y,na.rm=TRUE)

      if(minsc<mins) mins<-minsc
      if(minbc<minb) minb<-minbc
      if(maxsc>maxs) maxs<-maxsc
      if(maxbc>maxb) maxb<-maxbc      #mins etc are min max for all zeroes for one community

      dat<-cbind(x,y)

      # bagplot(dat,na.rm=TRUE,factor=2.5,create.plot=TRUE,approx.limit=300,
      #         show.outlier=TRUE,show.looppoints=TRUE,
      #         show.bagpoints=TRUE,dkmethod=2,
      #         show.whiskers=TRUE,show.loophull=TRUE,
      #         show.baghull=TRUE,verbose=FALSE,
      #         transparency=TRUE,
      #         ylab=paste0("Intercept"), xlab=paste("1-slope (-Amplitude relative to Reference",colnames(slope)[refindex],")"),cex=0.8,
      #         xlim=xlim,ylim=ylim,
      #         main=paste("Community ",levels(community.f)[j],"zero=",round(zero,digits=2),main,"\nzero event is" ,zname))
      #
      # lines(seq(-20,20,by= 0.01),rep(0,4001))
      # lines(rep(0,2001),seq(-1000,1000, by=1))
      } #no plots done run each event separate (for each year/community) scale for each year
      if(mins<minsall) minsall<-mins
      if(minbc<minball) minball<-minb
      if(maxs>maxsall) maxsall<-maxs
      if(maxb>maxball) maxball<-maxb

   #   if(is.null(rownum)){
      if(length(which(!is.na(a)))!=0){
        names(a)<-names(c)<-names(r2)<-names(aerror)<-names(cerror)<-names(z)<-rownames(Td$ET.x)[ir]
        cat("\n\nGROUPING ",levels(community.f)[j])

        cat("\nlinear fits for \n",rownames(Td$ET.x)[ir], "\n is a=\n");
        print(a);

        # cat(" a+z-l.s.zero=\n");print(a+z-Td$l.s.zero);cat(" c=\n");print(c);cat("r2\n");print(r2);
        # cat(" aerror=\n");print(aerror);cat(" cerror=\n");print(cerror);cat("zero\n");print(z)
        cat("stats on linear fit a (slope)ERROR  a+z=constant=intersection point STD DEVa")
        valaerror<-nastat(aerror)
        cat("stats on linear fit c (intercept)ERROR  Group shift STD DEV")
        valcerror<-nastat(cerror)
        cat("\nStats on all Intercept c=Group displacements ")
        valA<-nastat(c)  #find a*(1-1)+c for all a,c values
        cat("stats on all a+z=constant=intersection point of Average profile ")
        valY<-nastat(a+z)
        cat("stats on Number of points ")
        valN<-nastat(N)
        cat("stats on all Intercept Probability for no correlation ")
        valp<-nastat(p)

        cat("\n Statistical value for Group intersection point(shift from intersection of ",
            colnames(Td$ET.x)[refindex]," profile) is ",valA$m , " std dev(ALL) ",valcerror$m, " N ",valN$m, " p ",valp$m)

        #cat("\n            intersection point(shift from average profile)  is ",valA$m +Td$l.s.zero, " std dev(ALL) ",valaerror$m,"\n")
        cat("\n Statistical value for Reference intersection point for ",colnames(Td$ET.x)[refindex]," is ",valY$m , " std dev(ALL) ",valaerror$m)
        cat("\n            intersection point(from Average value of profile)     is ",valY$m -Td$l.s.zero, " std dev ",valaerror$m,"\n")
        cat("\nAverage profile is \n"); print(Td$ET.x[,refindex])
        cat("\nvalY",valY$m)
        cat("\nir",ir,"\n")
        cat(Td$ET.x[,refindex],"\nenter")
        bestintersectr<-findbest(val=valY$m,comparelist=Td$ET.x[,refindex],ir=ir)
        vlow<-valY$m-valaerror$m
        vupper<-valY$m+valaerror$m
        cat(vlow,"\nenter")
        bestintersectlow<-findbest(val=vlow,comparelist=Td$ET.x[,refindex],ir=ir)
        cat(vupper,"\nenter")
        bestintersectup<-findbest(val=vupper,comparelist=Td$ET.x[,refindex],ir=ir)


        cat("\n\nSUMMARY: GROUPING ",levels(community.f)[j])
        if(!is.null(bestintersectr) ){
          cat("\n best intersection index is",bestintersectr," at amounts (low,mid,upper)", vlow,valY$m,vupper,
              "\n event (low mid upper) ",
              rownames(Td$ET.x)[bestintersectlow],rownames(Td$ET.x)[bestintersectr],rownames(Td$ET.x)[bestintersectup])
          bestintersectname<-c(bestintersectname,
                               rownames(Td$ET.x)[bestintersectlow],rownames(Td$ET.x)[bestintersectr],rownames(Td$ET.x)[bestintersectup])
          cat("\nGroup intersection point(shift from intersection of ",colnames(Td$ET.x)[refindex]," profile) is ",valA$m , " std dev(ALL) ",valcerror$m)
        } else{bestintersectname<-c(bestintersectname,NA,NA,NA); cat("\n No intersections found\n") }


      } else{
        bestintersectname<-c(bestintersectname,NA,NA,NA)
      }
        if (length(pslope)>1){
        cat("\nCommunity GROUPING ",levels(community.f)[j]," slope probability\n")
        print(pslope);cat("\n")


        maxpslope<-max(pslope,na.rm = TRUE)
        minpslope<-min(pslope,na.rm = TRUE)
        cat("\n peak in slope p is ",maxpslope," at ",names(pslope)[which(maxpslope==pslope)],"\n")
        cat("\n min in slope p is ",minpslope," at ",names(pslope)[which(minpslope==pslope)],"\n")
        cat("\n Best measure: ln[ Ratio of max/min of p slope] is ",log(maxpslope/minpslope),"\n")


        plot(pslope,
             main= paste(main,"\nCommunity ",levels(community.f)[j],
                              "Slope probability changing with zero as events"),
             ylab="Probability that slope is 0",pch=15,cex=1.9,cex.main=0.8,xaxt='n')
        axis(1,at=1:length(pslope),labels=names(pslope),lwd=2,cex.lab=0.8, cex.axis=0.8,  cex.sub=0.8)
        lines(1:length(pslope),rep(1,length(pslope)))
        legend("right",legend=paste("peak case:",names(pslope)[which(maxpslope==pslope)],
                                      "\nln(pmax/pmin)",round(log(maxpslope/minpslope),digits=1)))
        cat("\nSlope of b vs (1-a)\n")
        print(aslope);cat("\n")

        cat("\n peak in  probability (that slope is 0) is ",maxpslope," at ",names(pslope)[which(maxpslope==pslope)],"\n")
        cat("\n min in slope p is ",minpslope," at ",names(pslope)[which(minpslope==pslope)],"\n")
        cat("\n Best measure: ln[Ratio of max/min of p slope] is ",log(maxpslope/minpslope),"\n")
        plot(aslope,ylab="Slope  (intercept vs (1-a))",
             main=paste(main,"\nCommunity ",levels(community.f)[j],
                        "Slope changing zero as events"),pch=15,cex=1.9,cex.main=0.8,xaxt='n')
        axis(1,at=1:length(aslope),labels=names(aslope),lwd=2,cex.lab=0.8, cex.axis=0.8,  cex.sub=0.8)
        lines(1:length(aslope),rep(0,length(aslope)))
        }

      mixname<-paste0(levels(community.f)[j])
      names(bestintersectname)[(length(bestintersectname)-2):length(bestintersectname)]<-c(paste0("LOW",mixname),
                                                                                           paste0("MID",mixname),paste0("UPPER",mixname))
   #   }   #printouts if rownum NOT set

      if(!is.null(rownum)){
        for( irow in ir){
        zero<-Td$ET.x[irow,refindex]
        cat("\nFirst Start zero ",zero," with irow ",irow)
        zname<-rownames(Td$ET.x)[irow]
        newb<-reviseb_witherror(rownum=irow,Td=Td,ref=refindex)   # revise the zero based on zero=x[rownum,ref]  imagenan(newb$smat)
        slope<-newb$E.s   #colnames(slope)
        #print(newb$l.s.zero)
        #zero<-newb$l.s.zero
        intercept<-newb$E.b
        #subset and use the site data
        #x <- smat[i, which(orig[,indexcol]==levels(community.f)[j])]
        x <- 1-slope[ which(community.f==levels(community.f)[j]),refindex]
        y<-intercept[ which(community.f==levels(community.f)[j]),refindex]

        if(is.null(xlim)){
        ds<-maxs-mins; db<-maxb-minb
        slim<-c(mins-ds/4,maxs+ds/2)
        } else {
          slim<-xlim
        }
        if(is.null(ylim)){
          blim<-c(minb-db/4,maxb+db/4)
        } else {
          blim<-ylim
        }
        dat<-cbind(x,y)
        if(irow !=ir[1])par(new=TRUE)



        aplpack::bagplot(dat,na.rm=TRUE,factor=2.5,create.plot=TRUE,approx.limit=300,
                show.outlier=TRUE,show.looppoints=TRUE,
                show.bagpoints=TRUE,dkmethod=2,
                show.whiskers=TRUE,show.loophull=TRUE,
                show.baghull=TRUE,verbose=FALSE,
                col.looppoints=colourpt,
                col.loophull=transgrey,
                col.baghull=vcolours,

                ylab=paste0("Intercept"), xlab=paste("1-slope (-Amplitude relative to Reference",colnames(slope)[refindex],")"),cex=0.8,
                xlim=slim,ylim=blim,
                main=paste("Community ",levels(community.f)[j],main),cex.main=0.8)

        lines(seq(-20,20,by= 0.01),rep(0,4001))
        lines(rep(0,2001),seq(-1000,1000, by=1))    #transparency=TRUE,
      } # put all events together  (scale for each year/community)
      legend("topright",inset= 0.0,title="Events", rownames(Td$ET.x)[ir], fill=vcolours )
      } #only run if event rownum specifically set

    } #for each community (year) put all events together each year scale

    ds<-maxsall-minsall;db<-maxball-minball
    ;
    if(is.null(xlim)){
      slim<-c(minsall-ds/8,maxsall+ds/4)
    } else {
      slim<-xlim
    }
    if(is.null(ylim)){
      blim<-c(minball-db/4,maxball+db/8)
    } else {
      blim<-ylim
    }

    for(j in 1:length(levels(community.f))) {
    cat("\nmin max s",slim," min max b",blim,"\n")

    for( irow in ir){
      zero<-Td$ET.x[irow,refindex]
      #cat("\n Start",zero)
      zname<-rownames(Td$ET.x)[irow]


      newb<-reviseb_witherror(rownum=irow,Td=Td,ref=refindex)   # revise the zero based on zero=x[rownum,ref]  imagenan(newb$smat)
      slope<-newb$E.s   #colnames(slope)
      #print(newb$l.s.zero)
      #zero<-newb$l.s.zero
      intercept<-newb$E.b

      #subset and use the site data
      #x <- smat[i, which(orig[,indexcol]==levels(community.f)[j])]
      x <- 1-slope[ which(community.f==levels(community.f)[j]),refindex]
      y<-intercept[ which(community.f==levels(community.f)[j]),refindex]

      dat<-cbind(x,y)
      xl<-paste("1-slope (-Amplitude relative to Reference",colnames(slope)[refindex],")")
      yl<-paste0("Intercept")
      find_ellipse(p=dat,xlim=slim,ylim=blim,xl=xl,yl=yl,
                   main=paste("Community ",levels(community.f)[j],"zero=",round(zero,digits=2),main,"\nzero event is" ,zname))

      aplpack::bagplot(dat,na.rm=TRUE,factor=2.5,create.plot=TRUE,approx.limit=300,
              show.outlier=TRUE,show.looppoints=TRUE,
              show.bagpoints=TRUE,dkmethod=2,
              show.whiskers=TRUE,show.loophull=TRUE,
              show.baghull=TRUE,verbose=FALSE,
              transparency=TRUE,
              ylab=paste0("Intercept"), xlab=paste("1-slope (-Amplitude relative to Reference",colnames(slope)[refindex],")"),cex=0.8,
              xlim=slim,ylim=blim,
              main=paste("Community ",levels(community.f)[j],"zero=",round(zero,digits=2),main,"\nzero event is" ,zname),
              cex.main=0.8)

      lines(seq(-20,20,by= 0.01),rep(0,4001))
      lines(rep(0,2001),seq(-1000,1000, by=1))
    } #run each event separate with full scale range

    if(is.null(rownum)){
      irr<-1
    for( irow in ir){
      zero<-Td$ET.x[irow,refindex]
     # cat("\n Start",zero)
      zname<-rownames(Td$ET.x)[irow]
      newb<-reviseb_witherror(rownum=irow,Td=Td,ref=refindex)   # revise the zero based on zero=x[rownum,ref]  imagenan(newb$smat)
      slope<-newb$E.s   #colnames(slope)
     # print(newb$l.s.zero)
      #zero<-newb$l.s.zero
      intercept<-newb$E.b
      #subset and use the site data
      #x <- smat[i, which(orig[,indexcol]==levels(community.f)[j])]
      x <- 1-slope[ which(community.f==levels(community.f)[j]),refindex]
      y<-intercept[ which(community.f==levels(community.f)[j]),refindex]

      dat<-cbind(x,y)
      if(irow !=ir[1])par(new=TRUE)

      aplpack::bagplot(dat,na.rm=TRUE,factor=2.5,create.plot=TRUE,approx.limit=300,
              show.outlier=TRUE,show.looppoints=TRUE,
              show.bagpoints=TRUE,dkmethod=2,
              show.whiskers=TRUE,show.loophull=TRUE,
              show.baghull=TRUE,verbose=FALSE,
              col.looppoints=colourpt[irr],
              col.loophull=transgrey,
              col.baghull=vcolours[irr],
              col.outlier=colourpt[irr],
              ylab=paste0("Intercept"), xlab=paste("1-slope (-Amplitude relative to Reference",colnames(slope)[refindex],")"),cex=0.8,
              xlim=slim,ylim=blim,
              main=paste("Community ",levels(community.f)[j],main),cex.main=0.8)
      irr<-irr+1
      lines(seq(-20,20,by= 0.01),rep(0,4001))
      lines(rep(0,2001),seq(-1000,1000, by=1))    #transparency=TRUE,
    } #plot all zeroes together (same scale for each)
    legend("topright",inset= 0.0,title="Events", rownames(Td$ET.x)[ir], fill=vcolours )
    }  #only run if more than one event is in list ir
    }   #make bagplots


  ds<-maxsall-minsall; db<-maxball-minball

  if(is.null(xlim)){
    slim<-c(minsall-0*ds/8,maxsall+ds/4)
  } else {
    slim<-xlim
  }
  if(is.null(ylim)){
    blim<-c(minball-0*db/4,maxball+0*db/8)
  } else {
    blim<-ylim
  }
    } }

  pinter<-pslope<-NULL
  aslope<-NULL
  a<-NULL;c<-NULL;r2<-NULL;aerror<-NULL;cerror<-NULL;z<-NULL; N<-NULL;p<-NULL
  for( irow in ir){
    zero<-Td$ET.x[irow,refindex]     #   plot(Td$ET.x[,"Row_Ave"]-Td$l.s.zero)
    # ds<-maxsall-minsall; db<-maxball-minball
    # slim<-c(minsall-ds/8,maxsall+ds/4); blim<-c(minball-db/4,maxball+db/8)
    zname<-rownames(Td$ET.x)[irow]
    #cat("\n Start",zero, "rowname zero is ",zname)
    newb<-reviseb_witherror(rownum=irow,Td=Td,ref=refindex)   # revise the zero based on zero=x[rownum,ref]  imagenan(newb$smat)
    slope<-newb$E.s   #colnames(slope)
    #print(newb$l.s.zero)
    #zero<-newb$l.s.zero
    intercept<-newb$E.b
    x <- 1-slope[ ,refindex]
    y<-intercept[ ,refindex]                #all data together for each zero
    dat<-cbind(x,y)

    xl<-paste("1-slope (Amplitude)")
    yl<-paste0("Intercept")
    find_ellipse(p=dat,xlim=slim,ylim=blim,xl=xl,yl=yl,
                 main=paste("All data together\nReference",colnames(slope)[refindex],"zero =",round(zero,digits=2),
                            "zero event is" ,zname,main))

    aplpack::bagplot(dat, ylab=paste0("Intercept"), xlab="1-slope (Amplitude)",cex=0.8,xlim=slim,ylim=blim,
            main=paste("All data together\nReference",colnames(slope)[refindex],"zero =",round(zero,digits=2),
                       "zero event is" ,zname,main),cex.main=0.8)
    lines(seq(-20,20,by= 0.01),rep(0,4001))
    lines(rep(0,2001),seq(-1000,1000, by=1))
    # bagplot(dat, ylab=paste0("Intercept"), xlab="1-slope (Amplitude)",cex=0.8,ylim=ylim,xlim=xlim,
    #         main=paste("Intercept vs (1-slope)\nReference",colnames(slope)[refindex],"zero =",round(zero,digits=2),
    #                    main,"\nzero event is" ,zname),cex.main=0.9)
    # lines(seq(-20,20,by= 0.01),rep(0,4001))
    # lines(rep(0,2001),seq(-1000,1000, by=1))
    ncomp<-length(which(complete.cases(y,x)))
    if(ncomp>minpoints && sd(x,na.rm=TRUE)>10^-9  && sd(y,na.rm=TRUE)>10^-9 ){
      fit<-lm(y~x, na.action=na.exclude )
      cat("\n\nLINEAR FIT vs 1-slope for ALL DATA",main,
          "zero used is ", zero," at ",zname, "\nONLY intercept,error and intercept probability important\n")
      print(summary(fit))
      fit_coef<-coef(summary(fit))

      r2.1<-summary(fit)$r.squared

      cat(" a+z=constant=intersection point",fit_coef[2,"Estimate"]+zero," STD DEVa",fit_coef[2,"Std. Error"])
       Y<-fit_coef[2,"Estimate"]+zero
      cat(" intercept of fit = Group shift",fit_coef["(Intercept)","Estimate"]," STD DEV",fit_coef["(Intercept)","Std. Error"])

      cat("\n  N ",ncomp, " p(intercept differs from 0) ",fit_coef["(Intercept)","Pr(>|t|)"])
      pinter<-c(pinter,fit_coef["(Intercept)","Pr(>|t|)"])
      names(pinter)[length(pinter)]<-zname
      pslope<-c(pslope,fit_coef["x","Pr(>|t|)"])
      names(pslope)[length(pslope)]<-zname
      cat("\n  ", " p(slope differs from 0) ",fit_coef["x","Pr(>|t|)"])
      aslope<-c(aslope,fit_coef[2,"Estimate"])
      names(aslope)[length(aslope)]<-zname
      #cat("\nir",ir,"\n")
      cat("\nY ",Y,"\nenter ")
      bestintersectr<-findbest(val=Y,comparelist=Td$ET.x[,refindex],ir=(1:nrow(Td$ET.x)))
      vlow<-fit_coef[2,"Estimate"]+zero-fit_coef[2,"Std. Error"]
      vupper<-fit_coef[2,"Estimate"]+zero+fit_coef[2,"Std. Error"]
      cat("\nlower " ,vlow)
      bestintersectlow<-findbest(val=vlow,comparelist=Td$ET.x[,refindex],ir=(1:nrow(Td$ET.x)))
      cat("\nupper ",vupper)
      bestintersectup<-findbest(val=vupper,comparelist=Td$ET.x[,refindex],ir=(1:nrow(Td$ET.x)))
      cat("\n\n intersection rownames are LOW MID UPPER ",
          rownames(Td$ET.x)[bestintersectlow],rownames(Td$ET.x)[bestintersectr],rownames(Td$ET.x)[bestintersectup],"\n\n")

        a<-c(a,fit_coef[2,"Estimate"])
        aerror<-c(aerror,sse=fit_coef[2,"Std. Error"])
        c<-c(c,fit_coef["(Intercept)","Estimate"])
        cerror<-c(cerror,fit_coef["(Intercept)","Std. Error"])
        r2<-c(r2,summary(fit)$r.squared)
        N<-c(N,ncomp)
        p<-c(p,fit_coef["(Intercept)","Pr(>|t|)"])
        z<-c(z,zero)



    } else {a<-c(a,NA);c<-c(c,NA);r2<-c(r2,NA);aerror<-c(aerror,NA);cerror<-c(cerror,NA);z<-c(z,NA)}



    if(!is.null(community.f)){
      length(levels(community.f))

      summary(community.f)
      # is set up for runs 150 to 241 but for long term data for Salix
      # for(i in 1:nrow(smat))

      par(mfrow=c(1,1))
      x <- 1-slope[ which(community.f==levels(community.f)[j]),refindex]
      y<-intercept[ which(community.f==levels(community.f)[j]),refindex]
      dat<-cbind(x,y)

      #colourpt=rainbow(length(levels(community.f)))
      #vcolours<-colourpt<-lotscol[1:length(levels(community.f))]
      vcolours<-lotscol1[1:length(levels(community.f))]
      colourpt<-lotscol[1:length(levels(community.f))]
      #colourpt2=rainbow(length(levels(f2)))

      #vcolours<-add.alpha(colourpt,translevel)
      #vcolours2<-add.alpha(colourpt2,translevel)
      transgrey<-add.alpha("grey",translevel)

      for(j in 1:length(levels(community.f))) {
        #subset and use the site data
        #x <- smat[i, which(orig[,indexcol]==levels(community.f)[j])]
        x <- 1-slope[ which(community.f==levels(community.f)[j]),refindex]
        y<-intercept[ which(community.f==levels(community.f)[j]),refindex]

        dat<-cbind(x,y)
        if(j !=1)par(new=TRUE)

        # xl<-paste("1-slope (-Amplitude relative to Ref)")
        # yl<-paste0("Intercept")
        # find_ellipse(p=dat,xlim=slim,ylim=blim,xl=xl,yl=yl,
        #              main=paste("Reference",colnames(slope)[refindex],"zero time=",round(zero,digits=2),
        #                         main,"zero event is" ,zname))

        aplpack::bagplot(dat,na.rm=TRUE,factor=2.5,create.plot=TRUE,approx.limit=300,
                show.outlier=TRUE,show.looppoints=TRUE,
                show.bagpoints=FALSE,dkmethod=2,
                show.whiskers=FALSE,show.loophull=TRUE,
                show.baghull=TRUE,verbose=FALSE,

                col.looppoints=colourpt[j],
                col.loophull=transgrey,
                col.baghull=vcolours[j],
                col.bagpoints="black",

                ylab=paste0("Intercept"), xlab="1-slope (-Amplitude relative to Ref)",
                main=paste("Reference",colnames(slope)[refindex],"zero time=",round(zero,digits=2),
                           main,"zero event is" ,zname),
                xlim=slim,ylim=blim,cex.main=0.9
        )
        # bagplot(dat,na.rm=TRUE,factor=2.5,create.plot=TRUE,approx.limit=300,
        #         col.looppoints=colourpt[j],
        #         col.loophull=transgrey,
        #         col.baghull=vcolours[j],
        #         col.bagpoints="black",
        #         ylab=paste0("Intercept"), xlab="1-slope (-Amplitude relative to Ref)",
        #         main=paste("Intercept vs (1-slope) Reference\n",colnames(slope)[refindex],"zero time=",round(zero,digits=2)),
        #         xlim=xlim,ylim=ylim
        # )

        lines(seq(-20,20,by= 0.01),rep(0,4001))
        lines(rep(0,2001),seq(-1000,1000, by=1))
      } #put all years on one plot (for each event)

      legend("topright",inset= 0.0,title="All", levels(community.f), fill=vcolours )
    }

  } #end of zero change loop  that first puts all years together and then divides them up

  if(length(ir)!=1){
    vcolours<-lotscol1[1:length(ir)]
    colourpt<-lotscol[1:length(ir)]
    iir<-1
  for( irow in ir){
    zero<-Td$ET.x[irow,refindex]
    # ds<-maxsall-minsall; db<-maxball-minball
    # slim<-c(minsall-ds/8,maxsall+ds/4); blim<-c(minball-db/4,maxball+db/8)
    zname<-rownames(Td$ET.x)[irow]
   # cat("\n Start",zero, "rowname zerro is ",zname)
    newb<-reviseb_witherror(rownum=irow,Td=Td,ref=refindex)   # revise the zero based on zero=x[rownum,ref]  imagenan(newb$smat)
    slope<-newb$E.s   #colnames(slope)
   # print(newb$l.s.zero)
    #zero<-newb$l.s.zero
    intercept<-newb$E.b
    x <- 1-slope[ ,refindex]
    y<-intercept[ ,refindex]                #all data together for each zero
    dat<-cbind(x,y)
    if(irow !=ir[1])par(new=TRUE)
    # bagplot(dat, ylab=paste0("Intercept"), xlab="1-slope (Amplitude)",cex=0.8,xlim=slim,ylim=blim,
    #         main=paste("Intercept vs (1-slope)\nReference",colnames(slope)[refindex],"zero =",round(zero,digits=2),
    #                    "zero event is" ,zname,main),cex.main=0.8)
    # lines(seq(-20,20,by= 0.01),rep(0,4001))
    # lines(rep(0,2001),seq(-1000,1000, by=1))
    # bagplot(dat, ylab=paste0("Intercept"), xlab="1-slope (Amplitude)",cex=0.8,ylim=blim,xlim=xlim,
    #         main=paste("Intercept vs (1-slope)\nReference",colnames(slope)[refindex],"zero =",round(zero,digits=2),
    #                    main,"\nzero event is" ,zname),cex.main=0.9)
    # lines(seq(-20,20,by= 0.01),rep(0,4001))
    # lines(rep(0,2001),seq(-1000,1000, by=1))

    # xl<-paste("1-slope (-Amplitude relative to Reference",colnames(slope)[refindex],")")
    # yl<-paste0("Intercept")
    # find_ellipse(p=dat,xlim=slim,ylim=blim,xl=xl,yl=yl,
    #              main=paste("All communities together",main))

    aplpack::bagplot(dat,na.rm=TRUE,factor=2.5,create.plot=TRUE,approx.limit=300,
            show.outlier=TRUE,show.looppoints=TRUE,
            show.bagpoints=TRUE,dkmethod=2,
            show.whiskers=TRUE,show.loophull=TRUE,
            show.baghull=TRUE,verbose=FALSE,
            col.looppoints=colourpt[iir],
            col.loophull=transgrey,
            col.baghull=vcolours[iir],

            ylab=paste0("Intercept"), xlab=paste("1-slope (-Amplitude relative to Reference",colnames(slope)[refindex],")"),cex=0.8,
            xlim=slim,ylim=blim,
            main=paste("All communities together",main),cex.main=0.8)
    iir<-iir+1
    lines(seq(-20,20,by= 0.01),rep(0,4001))
    lines(rep(0,2001),seq(-1000,1000, by=1))    #transparency=TRUE,
  } #   puts all years/communities together for each zero and plots them together with zeros having different colours
  legend("topright",inset= 0.0,title="Events", rownames(Td$ET.x)[ir], fill=vcolours )
  }
  if (length(pslope)>1){

    cat("\nrownum is",rownum)
   # if(is.null(rownum)){

      if(length(which(!is.na(a)))!=0){
        names(a)<-names(c)<-names(r2)<-names(aerror)<-names(cerror)<-names(z)<-rownames(Td$ET.x)[ir]
        cat("\n\nALL together ")

        # cat("\nlinear fits for \n",rownames(Td$ET.x)[ir], "\n is a=\n");
        # print(a);
        #
        # cat(" a+z-l.s.zero=\n");print(a+z-Td$l.s.zero);cat(" c=\n");print(c);cat("r2\n");print(r2);
        # cat(" aerror=\n");print(aerror);cat(" cerror=\n");print(cerror);cat("zero\n");print(z)
        cat("stats on linear fit a (slope)ERROR  a+z=constant=intersection point STD DEVa")
        valaerror<-nastat(aerror)
        cat("stats on linear fit c (intercept)ERROR  Group shift STD DEV")
        valcerror<-nastat(cerror)
        cat("\nStats on all Intercept c=Group displacements ")
        valA<-nastat(c)  #find a*(1-1)+c for all a,c values
        cat("stats on all a+z=constant=intersection point of Average profile ")
        valY<-nastat(a+z)
        cat("stats on Number of points ")
        valN<-nastat(N)
        cat("stats on all Intercept Probability for no correlation ")
        valp<-nastat(p)

        cat("\n Statistical value for Group intersection point(shift from intersection of ",
            colnames(Td$ET.x)[refindex]," profile) is ",valA$m , " std dev(ALL) ",valcerror$m, " N ",valN$m, " p ",valp$m)

        #cat("\n            intersection point(shift from average profile)  is ",valA$m +Td$l.s.zero, " std dev(ALL) ",valaerror$m,"\n")
        cat("\n Statistical value for Reference intersection point for ",colnames(Td$ET.x)[refindex]," is ",valY$m , " std dev(ALL) ",valaerror$m)
        cat("\n            intersection point(from Average value of profile)     is ",valY$m -Td$l.s.zero, " std dev ",valaerror$m,"\n")
        cat("\nAverage profile is \n"); print(Td$ET.x[,refindex])
        cat("\nvalY",valY$m)
        cat("\nir",ir,"\n")
        cat(Td$ET.x[,refindex],"\nenter")
        bestintersectr<-findbest(val=valY$m,comparelist=Td$ET.x[,refindex],ir=ir)
        vlow<-valY$m-valaerror$m
        vupper<-valY$m+valaerror$m
        cat(vlow,"\nenter")
        bestintersectlow<-findbest(val=vlow,comparelist=Td$ET.x[,refindex],ir=ir)
        cat(vupper,"\nenter")
        bestintersectup<-findbest(val=vupper,comparelist=Td$ET.x[,refindex],ir=ir)
        bestintersectname<-c(bestintersectname,
                             rownames(Td$ET.x)[bestintersectlow],rownames(Td$ET.x)[bestintersectr],
                             rownames(Td$ET.x)[bestintersectup])
        mixname<-"All_Together"
        names(bestintersectname)[(length(bestintersectname)-2):length(bestintersectname)]<-
          c(paste0("LOW",mixname),
          paste0("MID",mixname),paste0("UPPER",mixname))

        cat("\n\nSUMMARY: ALL together ")
        if(!is.null(bestintersectr) ){
          cat("\n best intersection index is",bestintersectr," at amounts (low,mid,upper)", vlow,valY$m,vupper,
              "\n event (low mid upper) ",
              rownames(Td$ET.x)[bestintersectlow],rownames(Td$ET.x)[bestintersectr],rownames(Td$ET.x)[bestintersectup])

          cat("\nGroup intersection point(shift from intersection of ",
              colnames(Td$ET.x)[refindex]," profile) is ",valA$m , " std dev(ALL) ",valcerror$m)
        }


      }
     # } #is.null rownum

  cat("\nAll together: slope probability\n")
  print(pslope);cat("\n")
  #plot(pslope, main= paste0(main, "\nAll together:slope probability changing zero as events\n",main),pch=15,cex=2)
  maxpslope<-max(pslope,na.rm = TRUE)
  minpslope<-min(pslope,na.rm = TRUE)
  plot(pslope,
       main= paste0( "All together:slope probability changing zero as events",main),
       ylab="Probability that slope is 0",pch=15,cex=1.9,cex.main=0.8,xaxt='n')
  axis(1,at=1:length(pslope),labels=names(pslope),lwd=2,cex.lab=0.8, cex.axis=0.8,  cex.sub=0.8)
  lines(1:length(pslope),rep(1,length(pslope)))
  legend("right",legend=paste("peak case:",names(pslope)[which(maxpslope==pslope)],
                                "\nln(pmax/pmin)",round(log(maxpslope/minpslope),digits=1)))
  cat("\nSlope of b vs (1-a)\n")
  print(aslope);cat("\n")

  cat("\n peak in  probability (that slope is 0) is ",maxpslope," at ",names(pslope)[which(maxpslope==pslope)],"\n")
  cat("\n min in slope p is ",minpslope," at ",names(pslope)[which(minpslope==pslope)],"\n")
  cat("\n Best measure: ln[Ratio of max/min of p slope] is ",log(maxpslope/minpslope),"\n")
  plot(aslope,ylab="Slope  (intercept vs (1-a))",
       main= paste0( "All together:slope changing zero as events",main),pch=15,cex=1.9,cex.main=0.8,xaxt='n')
  axis(1,at=1:length(aslope),labels=names(aslope),lwd=2,cex.lab=0.8, cex.axis=0.8,  cex.sub=0.8)

  lines(1:length(aslope),rep(0,length(aslope)))
  } #prints and plots info about probabilites and slopes
  if(is.null(community.f))bestintersectname<-rownames(Td$ET.x)[bestintersectr]
  return(bestintersectname )
} #makes bagplot of slopes and intercepts  (with factor colouring if desired)

findbest<-function(val,comparelist,ir){
  bestintersectr<-NULL
  if(!is.nan(val) && length(ir)>1){
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

a_b_2factorsbag<-function(community.f=NULL,subfactor=NULL,Td,listval=NULL,
                      refindex="Row_Ave",facname=NULL,facname1=NULL,
                      xlim=NULL ,ylim=NULL,main=" ",rownum=NULL){  #slope,intercept,
  if(is.null(listval)){
    listval<-1:nrow(Td$E.s)
  }
  if(length(listval)!=length(community.f)){
    cat("\n inconsistent lengths of factors and slope array\n")
    return()
  }
  bestintersectname<-NULL
  minpoints<-4 # more than 4 points needed to find linear fits to best event
  #lotscol<-colors()[c(24,94,26,130,121,96,97,49,47,417,256,8,33,90,142,144,653)]
  lotscol<-colors()[c(24,94,26,124,633,450,453,11,68,254,257,51,630,76,142,150,653)]
  translevel<-0.35
  lotscol1<-add.alpha(lotscol,translevel)
  # plot(1:length(lotscol),cex=3,col=lotscol,pch=15)
  # plot(1:length(lotscol),cex=3,col=lotscol1,pch=15)

  if(!is.numeric(refindex)){
    refindex<-which(colnames(Td$smat)==refindex)
  }
  op =
    par(mfrow =  c(1,1), mar = c(8,4.5, 4.5,4))
  if(is.null(rownum)){ #zero not set so run through all values for rows of Row_Ave  column
    if(nrow(Td$ET.x)>20)rinc<-ceiling(nrow(Td$ET.x)/20) else rinc=1
    ir<-seq(1,nrow(Td$ET.x),by=rinc)
    zerolist<-Td$ET.x[ir,refindex]
    vcolours<-lotscol1[1:length(ir)]
    colourpt<-lotscol[1:length(ir)]
  } else {
    if(is.numeric(rownum))ir<-rownum else ir<-which(rownames(Td$ET.x)==rownum)
    zerolist<-Td$ET.x[rownum,refindex]
    vcolours<-"blue"
    colourpt<-"blue"
  }
  cat("\nir is ",ir,"\nrownames are ",rownames(Td$ET.x)[ir],"\n")

  levc<-length(levels(community.f))
    levs<-length(levels(subfactor))
   if(levs>1 && levc>1){
    print(summary(subfactor))

     # levc<-length(levels(community.f))
      cat("\nmain factor\n")
      print(summary(community.f))
     slope<-Td$E.s[listval ,refindex]   #colnames(slope)
     bcol<-lotscol[3:(length(levels(community.f))+2)]
    boxplot(slope ~community.f*subfactor, na.action=na.exclude,col=bcol,lwd=2,notch=FALSE,
            main=paste("zero=",round(Td$l.s.zero,digits=1),"\n",facname,":",facname1, main),cex=1.0,cex.axis=0.7,
            ylab="Slope",cex.main=0.8,
            xlab=paste(facname,":",facname1),cex.lab=1.2)
    legend("topright",legend=levels(community.f),fill=bcol)
    cat("\n\n\nSlope Linear model ", " with ",levels(community.f),"\nRandom effect due to ",levels(subfactor),"\n\n")
    print(summary(lme(slope ~  community.f  ,random = ~ 1|subfactor ,  na.action=na.exclude)))
    cat("\n\n\nSlope Simple Linear model ", " with ",levels(community.f),"\nand ",levels(subfactor),"\n\n")
    if(length(levels(subfactor))<4){
    print(summary(lm(slope ~  community.f*subfactor  ,  na.action=na.exclude)))
    }
    for( irow in ir){
      zero<-Td$ET.x[irow,refindex]

      zname<-rownames(Td$ET.x)[irow]
     # cat("\n Start",zero,zname)
      newb<-reviseb_witherror(rownum=irow,Td=Td,ref=refindex)   # revise the zero based on zero=x[rownum,ref]  imagenan(newb$smat)
      intercept<-newb$E.b[ listval,refindex]
     # plot(intercept,main=zname)
    boxplot(intercept ~community.f*subfactor, na.action=na.exclude,col=bcol,lwd=2,notch=FALSE,
            main=paste("zero=",round(zero,digits=1),zname,"\n",facname,":",facname1, main),cex=1.0,cex.axis=0.7,ylab="Intercept",cex.main=0.8,
            xlab=paste(facname,":",facname1),cex.lab=1.2)
    legend("topright",legend=levels(community.f),fill=bcol)
    cat("\n\n\nIntercept Linear model with random effect ", " with ",levels(community.f)," Random effect due to ",levels(subfactor)," Event ",zname,"\n\n")
    print(summary(lme(intercept ~  community.f  ,random = ~ 1|subfactor ,  na.action=na.exclude)))
    if(length(levels(subfactor))<4){
    cat("\nIntercept Simple Linear model ",zname, " with ",levels(community.f)," and ",levels(subfactor)," Event ",zname,"\n")
    print(summary(lm(intercept ~  community.f*subfactor  ,  na.action=na.exclude)))
    # plot((lme(intercept ~  OTC  ,random = ~ 1|Year , data = dat, na.action=na.exclude)))
    # intervals(lme(intercept ~  OTC  ,random = ~ 1|Year , data = dat, na.action=na.exclude))
    }
    }

    #return()

    for(subj in 1:length(levels(subfactor))) {

       # same full scale for all years unless changed
    if(is.null(xlim)){
      ds<-maxs-mins; db<-maxb-minb
      slim<-c(mins-ds/4,maxs+ds/2)
      slim<-c(-1,1);
    } else {
      slim<-xlim
    }
    if(is.null(ylim)){
      blim<-c(minb-db/4,maxb+db/4)
      blim<-c(-30,30)
    } else {
      blim<-ylim
    }

  #run each factor/community for different zeroes but
  if(!is.null(community.f)){
    levc<-length(levels(community.f))
    if(levc>1){
    summary(community.f)
    # is set up for runs 150 to 241 but for long term data for Salix
    # for(i in 1:nrow(smat))
    # newb<-reviseb_witherror(rownum=irow,Td=Td,ref=refindex)   # revise the zero based on zero=x[rownum,ref]  imagenan(newb$smat)
    # slope<-newb$E.s   #colnames(slope)
    # print(newb$l.s.zero)
    # #zero<-newb$l.s.zero
    # intercept<-newb$E.b
    # par(mfrow=c(1,1))
    # x <- 1-slope[ which(community.f==levels(community.f)[j]),refindex]
    # y<-intercept[ which(community.f==levels(community.f)[j]),refindex]
    # dat<-cbind(x,y)
    maxsall<-maxball<-(-10^10); minsall<-minball<-10^10
    for(j in 1:length(levels(community.f))) {
      #first put all one one plot
      #colourpt=rainbow(length(ir))
      #vcolours<-colourpt<-lotscol[1:length(ir)]
      #colourpt2=rainbow(length(levels(f2)))

      #vcolours<-add.alpha(colourpt,translevel)
      #vcolours2<-add.alpha(colourpt2,translevel)
      transgrey<-add.alpha("grey",translevel)
     # cat("\nir = ",ir,"\n")

      maxs<-maxb<-(-10^10); mins<-minb<-10^10
      pinter<-pslope<-NULL
      aslope<-NULL
      a<-NULL;c<-NULL;r2<-NULL;aerror<-NULL;cerror<-NULL;z<-NULL;N<-NULL;p<-NULL
      for( irow in ir){
        zero<-Td$ET.x[irow,refindex]
        #cat("\n Start",zero)
        zname<-rownames(Td$ET.x)[irow]


        newb<-reviseb_witherror(rownum=irow,Td=Td,ref=refindex)   # revise the zero based on zero=x[rownum,ref]  imagenan(newb$smat)
        slope<-newb$E.s   #colnames(slope)
       # print(newb$l.s.zero)
        #zero<-newb$l.s.zero
        intercept<-newb$E.b

        #subset and use the site data
        #x <- smat[i, which(orig[,indexcol]==levels(community.f)[j])]
        x <- 1-slope[ which(community.f==levels(community.f)[j] & subfactor==levels(subfactor)[subj] ),refindex]
        y<-intercept[ which(community.f==levels(community.f)[j] & subfactor==levels(subfactor)[subj] ),refindex]

        ncomp<-length(which(complete.cases(y,x)))
        if(ncomp>minpoints && sd(x,na.rm=TRUE)>10^-9  && sd(y,na.rm=TRUE)>10^-9 ){
          fit<-lm(y~x, na.action=na.exclude )
          fit_coef<-coef(summary(fit))
        a<-c(a,fit_coef[2,"Estimate"])
        aerror<-c(aerror,sse=fit_coef[2,"Std. Error"])
        c<-c(c,fit_coef["(Intercept)","Estimate"])
        cerror<-c(cerror,fit_coef["(Intercept)","Std. Error"])
        r2<-c(r2,summary(fit)$r.squared)
       z<-c(z,zero)
       N<-c(N,ncomp)
       p<-c(p,fit_coef["(Intercept)","Pr(>|t|)"])

       pinter<-c(pinter,fit_coef["(Intercept)","Pr(>|t|)"])
       names(pinter)[length(pinter)]<-zname
       pslope<-c(pslope,fit_coef["x","Pr(>|t|)"])
       names(pslope)[length(pslope)]<-zname
       cat("\n  ", " p(slope differs from 0) ",fit_coef["x","Pr(>|t|)"])
       aslope<-c(aslope,fit_coef[2,"Estimate"])
       names(aslope)[length(aslope)]<-zname

        } else {a<-c(a,NA);c<-c(c,NA);r2<-c(r2,NA);aerror<-c(aerror,NA);cerror<-c(cerror,NA);z<-c(z,NA)}



        maxsc<-max(x,na.rm=TRUE)
        maxbc<-max(y,na.rm=TRUE)
        minsc<-min(x,na.rm=TRUE)
        minbc<-min(y,na.rm=TRUE)

        if(minsc<mins) mins<-minsc
        if(minbc<minb) minb<-minbc
        if(maxsc>maxs) maxs<-maxsc
        if(maxbc>maxb) maxb<-maxbc      #mins etc are min max for all zeroes for one community

        dat<-cbind(x,y)

        # bagplot(dat,na.rm=TRUE,factor=2.5,create.plot=TRUE,approx.limit=300,
        #         show.outlier=TRUE,show.looppoints=TRUE,
        #         show.bagpoints=TRUE,dkmethod=2,
        #         show.whiskers=TRUE,show.loophull=TRUE,
        #         show.baghull=TRUE,verbose=FALSE,
        #         transparency=TRUE,
        #         ylab=paste0("Intercept"), xlab=paste("1-slope (-Amplitude relative to Reference",colnames(slope)[refindex],")"),cex=0.8,
        #         xlim=xlim,ylim=ylim,
        #         main=paste("Community ",levels(community.f)[j],"zero=",round(zero,digits=2),main,"\nzero event is" ,zname))
        #
        # lines(seq(-20,20,by= 0.01),rep(0,4001))
        # lines(rep(0,2001),seq(-1000,1000, by=1))
      } #no plots done run each event separate (for each year/community) scale for each year
      if(length(which(!is.na(a)))!=0){
      names(a)<-names(c)<-names(r2)<-names(aerror)<-names(cerror)<-names(z)<-rownames(Td$ET.x)[ir]
      cat("\n\nGROUPING(Inner Loop) ",levels(community.f)[j],"SUB GROUP(outer loop)",levels(subfactor)[subj])

      cat("\nlinear fits for \n",rownames(Td$ET.x)[ir], "\n is a=\n");
      print(a);
      # cat(" a+z-l.s.zero=\n");print(a+z-Td$l.s.zero);cat(" c=\n");print(c);cat("r2\n");print(r2);
      # cat(" aerror=\n");print(aerror);cat(" cerror=\n");print(cerror);cat("zero\n");print(z)
      cat("stats on linear fit a (slope)ERROR  a+z=constant=intersection point STD DEVa")
      valaerror<-nastat(aerror)
      cat("stats on linear fit c (intercept)ERROR  Group shift STD DEV")
      valcerror<-nastat(cerror)
      cat("\nStats on all Intercept c=Group displacements ")
      valA<-nastat(c)  #find a*(1-1)+c for all a,c values
      cat("stats on all a+z=constant=intersection point of Average profile ")
      valY<-nastat(a+z)
      cat("stats on all Number of points ")
      valN<-nastat(N)
      cat("stats on all Intercept Probability for no correlation ")
      valp<-nastat(p)
      cat("\n Statistical value for Group intersection point(shift from intersection of ",
          colnames(Td$ET.x)[refindex]," profile) is ",valA$m , " std dev(ALL) ",valcerror$m, " N ",valN$m, " p ",valp$m)
      #cat("\n            intersection point(shift from average profile)  is ",valA$m +Td$l.s.zero, " std dev(ALL) ",valaerror$m,"\n")
      cat("\n Statistical value for Reference intersection point for ",colnames(Td$ET.x)[refindex]," is ",valY$m , " std dev(ALL) ",valaerror$m)
      cat("\n            intersection point(from Average value of profile)     is ",valY$m -Td$l.s.zero, " std dev ",valaerror$m,"\n")
      cat("\nAverage profile is \n"); print(Td$ET.x[,refindex])
      #bestintersectr<-NULL
      #  if(!is.nan(valY$m)){
      # for(r in 1:(length(ir)-1)){
      #   dist<-abs(valY$m-Td$ET.x[ir[r],refindex])
      #   disttot<-abs(Td$ET.x[ir[r+1],refindex]-Td$ET.x[ir[r],refindex])
      #   if(dist<disttot && valY$m>=Td$ET.x[ir[r],refindex] ){if(dist<disttot/2)bestintersectr<-(r) else bestintersectr<-(r+1)}
      #   }
      # if(is.null(bestintersectr)){
      #   if(valY$m<Td$ET.x[ir[1],refindex])bestintersectr<-1 else bestintersectr<-length(ir)
      # }
      # }
    bestintersectr<-findbest(val=valY$m,comparelist=Td$ET.x[,refindex],ir=ir)
 vlow<-valY$m-valaerror$m
 vupper<-valY$m+valaerror$m
    bestintersectlow<-findbest(val=vlow,comparelist=Td$ET.x[,refindex],ir=ir)
    bestintersectup<-findbest(val=vupper,comparelist=Td$ET.x[,refindex],ir=ir)


      cat("\n\nSUMMARY: GROUPING(Inner Loop) ",levels(community.f)[j],"SUB GROUP(outer loop)",levels(subfactor)[subj])
      if(!is.null(bestintersectr) ){
        cat("\n best intersection index is",bestintersectr," at amounts (low,mid,upper)", vlow,valY$m,vupper,
            "\n event (low mid upper) ",
            rownames(Td$ET.x)[bestintersectlow],rownames(Td$ET.x)[bestintersectr],rownames(Td$ET.x)[bestintersectup])
      bestintersectname<-c(bestintersectname,
                           rownames(Td$ET.x)[bestintersectlow],rownames(Td$ET.x)[bestintersectr],rownames(Td$ET.x)[bestintersectup])
      cat("\nGroup intersection point(shift from intersection of ",colnames(Td$ET.x)[refindex]," profile) is ",valA$m , " std dev(ALL) ",valcerror$m)
      } else{bestintersectname<-c(bestintersectname,NA,NA,NA) }


      } else{
        bestintersectname<-c(bestintersectname,NA,NA,NA)
      }

      if (length(pslope)>1){
        cat("\nCommunity GROUPING(Inner Loop) ",levels(community.f)[j],"SUB GROUP(outer loop)",levels(subfactor)[subj],
            " slope probability\n")
        print(pslope);cat("\n")

        maxpslope<-max(pslope,na.rm = TRUE)
        minpslope<-min(pslope,na.rm = TRUE)
        plot(pslope,
             main= paste(main,"\nCommunity",levels(community.f)[j],"SUB",levels(subfactor)[subj],
                              "\nProbability (slope=0) changing zero as event"),
             ylab="Probability that slope is 0",pch=15,cex=1.9,cex.main=0.8,xaxt='n')
        axis(1,at=1:length(pslope),labels=names(pslope),lwd=2,cex.lab=0.8, cex.axis=0.8,  cex.sub=0.8)
        lines(1:length(pslope),rep(1,length(pslope)))
        legend("right",legend=paste("peak case:",names(pslope)[which(maxpslope==pslope)],
                                      "\nln(pmax/pmin)",round(log(maxpslope/minpslope),digits=1)))
        cat("\nSlope of b vs (1-a)\n")
        print(aslope);cat("\n")

        cat("\n peak in  probability (that slope is 0) is ",maxpslope," at ",names(pslope)[which(maxpslope==pslope)],"\n")
        cat("\n min in slope p is ",minpslope," at ",names(pslope)[which(minpslope==pslope)],"\n")
        cat("\n Best measure: ln[Ratio of max/min of p slope] is ",log(maxpslope/minpslope),"\n")
        plot(aslope,ylab="Slope  (intercept vs (1-a))",
             main= paste(main,"Community",levels(community.f)[j],"SUB",levels(subfactor)[subj],
                         "\nSlope (intercept vs (1-a)) changing zero as event"),pch=15,cex=1.9,cex.main=0.8,xaxt='n')
        axis(1,at=1:length(aslope),labels=names(aslope),lwd=2,cex.lab=0.8, cex.axis=0.8,  cex.sub=0.8)
        lines(1:length(aslope),rep(0,length(aslope)))
      }

      mixname<-paste0(levels(community.f)[j],"_",levels(subfactor)[subj])
      names(bestintersectname)[(length(bestintersectname)-2):length(bestintersectname)]<-c(paste0("LOW",mixname),
                                                                                           paste0("MID",mixname),paste0("UPPER",mixname))
      #return(bestintersectname )

      if(mins<minsall) minsall<-mins
      if(minbc<minball) minball<-minb
      if(maxs>maxsall) maxsall<-maxs
      if(maxb>maxball) maxball<-maxb

      if(!is.null(rownum)){
        for( irow in ir){
          zero<-Td$ET.x[irow,refindex]
          #cat("\nFirst Start zero ",zero," with irow ",irow)
          zname<-rownames(Td$ET.x)[irow]
          newb<-reviseb_witherror(rownum=irow,Td=Td,ref=refindex)   # revise the zero based on zero=x[rownum,ref]  imagenan(newb$smat)
          slope<-newb$E.s   #colnames(slope)
          #print(newb$l.s.zero)
          #zero<-newb$l.s.zero
          intercept<-newb$E.b
          #subset and use the site data
          #x <- smat[i, which(orig[,indexcol]==levels(community.f)[j])]
          # x <- 1-slope[ which(community.f==levels(community.f)[j]),refindex]
          # y<-intercept[ which(community.f==levels(community.f)[j]),refindex]
          x <- 1-slope[ which(community.f==levels(community.f)[j] & subfactor==levels(subfactor)[subj] ),refindex]
          y<-intercept[ which(community.f==levels(community.f)[j] & subfactor==levels(subfactor)[subj] ),refindex]
          ds<-maxs-mins; db<-maxb-minb

          if(is.null(xlim)){
            slim<-c(mins-ds/4,maxs+ds/2)
          } else {
            slim<-xlim
          }
          if(is.null(ylim)){
            blim<-c(minb-db/4,maxb+db/4)
          } else {
            blim<-ylim
          }
          dat<-cbind(x,y)
          if(irow !=ir[1])par(new=TRUE)

          aplpack::bagplot(dat,na.rm=TRUE,factor=2.5,create.plot=TRUE,approx.limit=300,
                  show.outlier=TRUE,show.looppoints=TRUE,
                  show.bagpoints=TRUE,dkmethod=2,
                  show.whiskers=TRUE,show.loophull=TRUE,
                  show.baghull=TRUE,verbose=FALSE,
                  col.looppoints=colourpt,
                  col.loophull=transgrey,
                  col.baghull=vcolours,

                  ylab=paste0("Intercept"), xlab=paste("1-slope (-Amplitude relative to Reference",colnames(slope)[refindex],")"),cex=0.8,
                  xlim=slim,ylim=blim,
                  main=paste("Subfactor",levels(subfactor)[subj],"Community ",levels(community.f)[j],main),cex.main=0.8)

          lines(seq(-20,20,by= 0.01),rep(0,4001))
          lines(rep(0,2001),seq(-1000,1000, by=1))    #transparency=TRUE,
        } # put all events together  (scale for each year/community)
        legend("topright",inset= 0.0,title="Events", rownames(Td$ET.x)[ir], fill=vcolours )
      } #only run if event rownum specifically set

    } #for each year put all events together each year scale

    ds<-maxsall-minsall; db<-maxball-minball

    if(is.null(xlim)){
      slim<-c(minsall-ds/8,maxsall+ds/4);
    } else {
      slim<-xlim
    }
    if(is.null(ylim)){
      blim<-c(minball-db/4,maxball+db/8)
    } else {
      blim<-ylim
    }

    for(j in 1:length(levels(community.f))) {
     # cat("\nmin max s",slim," min max b",blim,"\n")

      for( irow in ir){
        zero<-Td$ET.x[irow,refindex]
      # cat("\n Start",zero)
        zname<-rownames(Td$ET.x)[irow]


        newb<-reviseb_witherror(rownum=irow,Td=Td,ref=refindex)   # revise the zero based on zero=x[rownum,ref]  imagenan(newb$smat)
        slope<-newb$E.s   #colnames(slope)
      #  print(newb$l.s.zero)
        #zero<-newb$l.s.zero
        intercept<-newb$E.b

        #subset and use the site data
        #x <- smat[i, which(orig[,indexcol]==levels(community.f)[j])]
        # x <- 1-slope[ which(community.f==levels(community.f)[j]),refindex]
        # y<-intercept[ which(community.f==levels(community.f)[j]),refindex]
        x <- 1-slope[ which(community.f==levels(community.f)[j] & subfactor==levels(subfactor)[subj] ),refindex]
        y<-intercept[ which(community.f==levels(community.f)[j] & subfactor==levels(subfactor)[subj] ),refindex]
        dat<-cbind(x,y)

        xl<-paste("1-slope (-Amplitude relative to Reference",colnames(slope)[refindex],")")
        yl<-paste0("Intercept")
        find_ellipse(p=dat,xlim=slim,ylim=blim,xl=xl,yl=yl,
                     main=paste("Subfactor",levels(subfactor)[subj],"Community ",levels(community.f)[j],
                                "zero=",round(zero,digits=2),main,"\nzero event is" ,zname))

        aplpack::bagplot(dat,na.rm=TRUE,factor=2.5,create.plot=TRUE,approx.limit=300,
                show.outlier=TRUE,show.looppoints=TRUE,
                show.bagpoints=TRUE,dkmethod=2,
                show.whiskers=TRUE,show.loophull=TRUE,
                show.baghull=TRUE,verbose=FALSE,
                transparency=TRUE,
                ylab=paste0("Intercept"), xlab=paste("1-slope (-Amplitude relative to Reference",colnames(slope)[refindex],")"),cex=0.8,
                xlim=slim,ylim=blim,
                main=paste("Subfactor",levels(subfactor)[subj],"Community ",levels(community.f)[j],
                           "zero=",round(zero,digits=2),main,"\nzero event is" ,zname),cex.main=0.8)

        lines(seq(-20,20,by= 0.01),rep(0,4001))
        lines(rep(0,2001),seq(-1000,1000, by=1))
      } #run each event separate with full scale range

     # if(is.null(rownum)){

        for( irow in ir){
          zero<-Td$ET.x[irow,refindex]
          #cat("\n Start",zero)
          zname<-rownames(Td$ET.x)[irow]
          newb<-reviseb_witherror(rownum=irow,Td=Td,ref=refindex)   # revise the zero based on zero=x[rownum,ref]  imagenan(newb$smat)
          slope<-newb$E.s   #colnames(slope)
         # print(newb$l.s.zero)
          #zero<-newb$l.s.zero
          intercept<-newb$E.b
          #subset and use the site data
          #x <- smat[i, which(orig[,indexcol]==levels(community.f)[j])]
          # x <- 1-slope[ which(community.f==levels(community.f)[j]),refindex]
          # y<-intercept[ which(community.f==levels(community.f)[j]),refindex]
          x <- 1-slope[ which(community.f==levels(community.f)[j] & subfactor==levels(subfactor)[subj] ),refindex]
          y<-intercept[ which(community.f==levels(community.f)[j] & subfactor==levels(subfactor)[subj] ),refindex]
          dat<-cbind(x,y)
          if(irow !=ir[1])par(new=TRUE)

          aplpack::bagplot(dat,na.rm=TRUE,factor=2.5,create.plot=TRUE,approx.limit=300,
                  show.outlier=TRUE,show.looppoints=TRUE,
                  show.bagpoints=TRUE,dkmethod=2,
                  show.whiskers=TRUE,show.loophull=TRUE,
                  show.baghull=TRUE,verbose=FALSE,
                  col.looppoints=colourpt[irow],
                  col.loophull=transgrey,
                  col.baghull=vcolours[irow],
                  col.outlier=colourpt[irow],
                  ylab=paste0("Intercept"), xlab=paste("1-slope (-Amplitude relative to Reference",colnames(slope)[refindex],")"),cex=0.8,
                  xlim=slim,ylim=blim,
                  main=paste("Subfactor",levels(subfactor)[subj],"Community ",levels(community.f)[j],main),cex.main=0.8)

          lines(seq(-20,20,by= 0.01),rep(0,4001))
          lines(rep(0,2001),seq(-1000,1000, by=1))    #transparency=TRUE,
        } #plot all zeroes together (same scale for each)
        legend("topright",inset= 0.0,title="Events", rownames(Td$ET.x)[ir], fill=vcolours )
      #}  #only run isnull rownum
    }


    ds<-maxsall-minsall; db<-maxball-minball

    if(is.null(xlim)){
      slim<-c(minsall-0*ds/8,maxsall+ds/4);
    } else {
      slim<-xlim
    }
    if(is.null(ylim)){
      blim<-c(minball-0*db/4,maxball+0*db/8)
    } else {
      blim<-ylim
    }

    }}


  for( irow in ir){
    zero<-Td$ET.x[irow,refindex]
    # ds<-maxsall-minsall; db<-maxball-minball
    # slim<-c(minsall-ds/8,maxsall+ds/4); blim<-c(minball-db/4,maxball+db/8)
    zname<-rownames(Td$ET.x)[irow]
    #cat("\n Start",zero, "rowname zerro is ",zname)
    newb<-reviseb_witherror(rownum=irow,Td=Td,ref=refindex)   # revise the zero based on zero=x[rownum,ref]  imagenan(newb$smat)
    slope<-newb$E.s   #colnames(slope)
    #print(newb$l.s.zero)
    #zero<-newb$l.s.zero
    intercept<-newb$E.b
    # x <- 1-slope[ ,refindex]
    # y<-intercept[ ,refindex]                #all data together for each zero
    x <- 1-slope[ which( subfactor==levels(subfactor)[subj] ),refindex]
    y<-intercept[ which(subfactor==levels(subfactor)[subj] ),refindex]
    dat<-cbind(x,y)

    xl<-"1-slope (Amplitude)"
    yl<-paste0("Intercept")
    find_ellipse(p=dat,xlim=slim,ylim=blim,xl=xl,yl=yl,
                 main=paste("Subfactor",levels(subfactor)[subj],"\nReference",colnames(slope)[refindex],"zero =",round(zero,digits=2),
                            "zero event is" ,zname,main))

    aplpack::bagplot(dat, ylab=paste0("Intercept"), xlab="1-slope (Amplitude)",cex=0.8,xlim=slim,ylim=blim,
            main=paste("Subfactor",levels(subfactor)[subj],"\nReference",colnames(slope)[refindex],"zero =",round(zero,digits=2),
                       "zero event is" ,zname,main),cex.main=0.8)
    lines(seq(-20,20,by= 0.01),rep(0,4001))
    lines(rep(0,2001),seq(-1000,1000, by=1))
    # bagplot(dat, ylab=paste0("Intercept"), xlab="1-slope (Amplitude)",cex=0.8,ylim=ylim,xlim=xlim,
    #         main=paste("Intercept vs (1-slope)\nReference",colnames(slope)[refindex],"zero =",round(zero,digits=2),
    #                    main,"\nzero event is" ,zname),cex.main=0.9)
    # lines(seq(-20,20,by= 0.01),rep(0,4001))
    # lines(rep(0,2001),seq(-1000,1000, by=1))

    if(!is.null(community.f)){
      length(levels(community.f))
      if(levc>1){
      summary(community.f)
      # is set up for runs 150 to 241 but for long term data for Salix
      # for(i in 1:nrow(smat))

      par(mfrow=c(1,1))
      # x <- 1-slope[ which(community.f==levels(community.f)[j]),refindex]
      # y<-intercept[ which(community.f==levels(community.f)[j]),refindex]
      x <- 1-slope[ which(community.f==levels(community.f)[j] & subfactor==levels(subfactor)[subj] ),refindex]
      y<-intercept[ which(community.f==levels(community.f)[j] & subfactor==levels(subfactor)[subj] ),refindex]
      dat<-cbind(x,y)

      #colourpt=rainbow(length(levels(community.f)))
      #vcolours<-colourpt<-lotscol[1:length(levels(community.f))]
      vcolours<-lotscol1[1:length(levels(community.f))]
      colourpt<-lotscol[1:length(levels(community.f))]
      #colourpt2=rainbow(length(levels(f2)))

      #vcolours<-add.alpha(colourpt,translevel)
      #vcolours2<-add.alpha(colourpt2,translevel)
      transgrey<-add.alpha("grey",translevel)

      for(j in 1:length(levels(community.f))) {
        #subset and use the site data
        #x <- smat[i, which(orig[,indexcol]==levels(community.f)[j])]
        # x <- 1-slope[ which(community.f==levels(community.f)[j]),refindex]
        # y<-intercept[ which(community.f==levels(community.f)[j]),refindex]
        x <- 1-slope[ which(community.f==levels(community.f)[j] & subfactor==levels(subfactor)[subj] ),refindex]
        y<-intercept[ which(community.f==levels(community.f)[j] & subfactor==levels(subfactor)[subj] ),refindex]
        dat<-cbind(x,y)
        if(j !=1)par(new=TRUE)

        # xl<-"1-slope (-Amplitude relative to Ref)"
        # yl<-paste0("Intercept")
        # find_ellipse(p=dat,xlim=slim,ylim=blim,xl=xl,yl=yl,
        #              main=paste("Reference",colnames(slope)[refindex],"zero time=",round(zero,digits=2),
        #                         main,"zero event is" ,zname,"Subfactor",levels(subfactor)[subj]))

        aplpack::bagplot(dat,na.rm=TRUE,factor=2.5,create.plot=TRUE,approx.limit=300,
                show.outlier=TRUE,show.looppoints=TRUE,
                show.bagpoints=FALSE,dkmethod=2,
                show.whiskers=FALSE,show.loophull=TRUE,
                show.baghull=TRUE,verbose=FALSE,

                col.looppoints=colourpt[j],
                col.loophull=transgrey,
                col.baghull=vcolours[j],
                col.bagpoints="black",

                ylab=paste0("Intercept"), xlab="1-slope (-Amplitude relative to Ref)",
                main=paste("Reference",colnames(slope)[refindex],"zero time=",round(zero,digits=2),
                           main,"zero event is" ,zname,"Subfactor",levels(subfactor)[subj]),
                xlim=slim,ylim=blim,cex.main=0.9
        )
        # bagplot(dat,na.rm=TRUE,factor=2.5,create.plot=TRUE,approx.limit=300,
        #         col.looppoints=colourpt[j],
        #         col.loophull=transgrey,
        #         col.baghull=vcolours[j],
        #         col.bagpoints="black",
        #         ylab=paste0("Intercept"), xlab="1-slope (-Amplitude relative to Ref)",
        #         main=paste("Intercept vs (1-slope) Reference\n",colnames(slope)[refindex],"zero time=",round(zero,digits=2)),
        #         xlim=xlim,ylim=ylim
        # )

        lines(seq(-20,20,by= 0.01),rep(0,4001))
        lines(rep(0,2001),seq(-1000,1000, by=1))
      } #put all years on one plot (for each event)

      legend("topright",inset= 0.0,title="All", levels(community.f), fill=vcolours )
    } }

  } #end of zero change loop  that first puts all years together and then divides them up

  if(length(ir)!=1){
    vcolours<-lotscol1[1:length(ir)]
    colourpt<-lotscol[1:length(ir)]
    for( irow in ir){
      zero<-Td$ET.x[irow,refindex]
      # ds<-maxsall-minsall; db<-maxball-minball
      # slim<-c(minsall-ds/8,maxsall+ds/4); blim<-c(minball-db/4,maxball+db/8)
      zname<-rownames(Td$ET.x)[irow]
      #cat("\n Start",zero, "rowname zerro is ",zname)
      newb<-reviseb_witherror(rownum=irow,Td=Td,ref=refindex)   # revise the zero based on zero=x[rownum,ref]  imagenan(newb$smat)
      slope<-newb$E.s   #colnames(slope)
      #print(newb$l.s.zero)
      #zero<-newb$l.s.zero
      intercept<-newb$E.b
      # x <- 1-slope[ ,refindex]
      # y<-intercept[ ,refindex]                #all data together for each zero
      x <- 1-slope[ which( subfactor==levels(subfactor)[subj] ),refindex]
      y<-intercept[ which(subfactor==levels(subfactor)[subj] ),refindex]
      dat<-cbind(x,y)
      if(irow !=ir[1])par(new=TRUE)
      # bagplot(dat, ylab=paste0("Intercept"), xlab="1-slope (Amplitude)",cex=0.8,xlim=slim,ylim=blim,
      #         main=paste("Intercept vs (1-slope)\nReference",colnames(slope)[refindex],"zero =",round(zero,digits=2),
      #                    "zero event is" ,zname,main),cex.main=0.8)
      # lines(seq(-20,20,by= 0.01),rep(0,4001))
      # lines(rep(0,2001),seq(-1000,1000, by=1))
      # bagplot(dat, ylab=paste0("Intercept"), xlab="1-slope (Amplitude)",cex=0.8,ylim=ylim,xlim=xlim,
      #         main=paste("Intercept vs (1-slope)\nReference",colnames(slope)[refindex],"zero =",round(zero,digits=2),
      #                    main,"\nzero event is" ,zname),cex.main=0.9)
      # lines(seq(-20,20,by= 0.01),rep(0,4001))
      # lines(rep(0,2001),seq(-1000,1000, by=1))
      aplpack::bagplot(dat,na.rm=TRUE,factor=2.5,create.plot=TRUE,approx.limit=300,
              show.outlier=TRUE,show.looppoints=TRUE,
              show.bagpoints=TRUE,dkmethod=2,
              show.whiskers=TRUE,show.loophull=TRUE,
              show.baghull=TRUE,verbose=FALSE,
              col.looppoints=colourpt[irow],
              col.loophull=transgrey,
              col.baghull=vcolours[irow],

              ylab=paste0("Intercept"), xlab=paste("1-slope (-Amplitude relative to Reference",colnames(slope)[refindex],")"),cex=0.8,
              xlim=slim,ylim=blim,
              main=paste("Subfactor",levels(subfactor)[subj],main),cex.main=0.8)

      lines(seq(-20,20,by= 0.01),rep(0,4001))
      lines(rep(0,2001),seq(-1000,1000, by=1))    #transparency=TRUE,
    } #   puts all years/communities together for each zero and plots them together with zeros having different colours
    legend("topright",inset= 0.0,title="Events", rownames(Td$ET.x)[ir], fill=vcolours )
  }
    }
   }

  return(bestintersectname )
} #makes bagplot of slopes and intercepts  (with 2 factors colouring if desired)


#output from the TransformE function  Td<-transformE(d, Ave=TRUE)
#summary(Td)
# Td contains list of matrices relevant to the Transform process
#Transforms are :Equitable =Td$ET.x  least squared=Td$l.s.x Equitable Transform based only on average column= Td$Ave.ET.x
#(access via Td$l.s.x etc)

# smat -original data set

#l.s. prefix indicates least squares result
#s=slope, b=intercept sse =std error of slope bse=std error of intercept
#r2=coef. of determination N # of points in fit pslope= prob. for no correlation node== indication if fit is due to a node
#l.s.s     l.s.sse                                                    m list used for transform
# l.s.b      l.s.bse   l.s.r2    l.s.N      l.s.pslope l.s.node l.s.p_par l.s.zero
#least sq parameters intercept ierror r2 N prop for no corr  node  prob for slope=1
# l.s.x= Least squared transform with std. dev. errors =l.s.xsd at each point
#l.s.x      l.s.xsd    l.s.Es l.s.Eb   l.s.Ep    : masked matrices included slope=Es  interceprt=Eb prob= Ep
# Equitable slopes (E.s) and intercepts (E.b) with convergence information (E.rtestxm,E.rtestbm) and errors (E.rtestxsd,E.rtestbsd)
#E.numrun is # runs to get convergence
# along with first iteration slopes/intercepts (E.s1,E.b1) and iterations with std. dev. errors  (E.sd1, E.bsd1)
# E.s        E.numrun    E.rtestxm   E.rtestxsd  E.s1       E.sd1  E.sN   E.snode
# E.b        E.numrun    E.rtestbm   E.rtestbsd  E.b1       E.bsd1  E.bN
# Equitable tranform (ET.x) with std. dev. errors at each point (ET.xsd) and #number of points averaged to get Transform value (ET.N)
# ET.x       ET.xsd    ET.N   ET.Es ET.Eb   ET.Ep             : masked matrices included slope=Es  interceprt=Eb prob= Ep
# Ave.ET.x   Ave.ET.xsd Ave.ET.N Ave.ET.Es Ave.ET.Eb   Ave.ET.Ep transform based on only average column: masked matrices included





