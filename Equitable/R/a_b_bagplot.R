a_b_bagplot <-
function(community.f=NULL,Td,
                      refindex="Row_Ave",xlim=NULL ,ylim=NULL,main=" ",rownum=NULL,facname=NULL){  #slope,intercept,
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
    if(nrow(Td$ET.x)>20)rinc<-ceiling((nrow(Td$ET.x))/(10)) else rinc=1
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

      ncomp<-length(which(complete.cases(y,x)))
      if(ncomp>minpoints){
        fit<-lm(y~x, na.action=na.exclude )
        fit_coef<-coef(summary(fit))
        a<-c(a,fit_coef[2,"Estimate"])
        aerror<-c(aerror,sse=fit_coef[2,"Std. Error"])
        c<-c(c,fit_coef["(Intercept)","Estimate"])
        cerror<-c(cerror,fit_coef["(Intercept)","Std. Error"])
        r2<-c(r2,summary(fit)$r.squared)
        N<-c(N,ncomp)
        p<-c(p,fit_coef["(Intercept)","Pr(>|t|)"])
        z<-c(z,zero)

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

      if(is.null(rownum)){
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
        } else{bestintersectname<-c(bestintersectname,NA,NA,NA) }


      } else{
        bestintersectname<-c(bestintersectname,NA,NA,NA)
      }
      mixname<-paste0(levels(community.f)[j])
      names(bestintersectname)[(length(bestintersectname)-2):length(bestintersectname)]<-c(paste0("LOW",mixname),
                                                                                           paste0("MID",mixname),paste0("UPPER",mixname))
      }

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

        ds<-maxs-mins; db<-maxb-minb
        slim<-c(mins-ds/4,maxs+ds/2); blim<-c(minb-db/4,maxb+db/4)
        dat<-cbind(x,y)
        if(irow !=ir[1])par(new=TRUE)

        bagplot(dat,na.rm=TRUE,factor=2.5,create.plot=TRUE,approx.limit=300,
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

    } #for each year put all events together each year scale

    ds<-maxsall-minsall; db<-maxball-minball
    slim<-c(minsall-ds/8,maxsall+ds/4); blim<-c(minball-db/4,maxball+db/8)

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

      bagplot(dat,na.rm=TRUE,factor=2.5,create.plot=TRUE,approx.limit=300,
              show.outlier=TRUE,show.looppoints=TRUE,
              show.bagpoints=TRUE,dkmethod=2,
              show.whiskers=TRUE,show.loophull=TRUE,
              show.baghull=TRUE,verbose=FALSE,
              transparency=TRUE,
              ylab=paste0("Intercept"), xlab=paste("1-slope (-Amplitude relative to Reference",colnames(slope)[refindex],")"),cex=0.8,
              xlim=slim,ylim=blim,
              main=paste("Community ",levels(community.f)[j],"zero=",round(zero,digits=2),main,"\nzero event is" ,zname),cex.main=0.8)

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

      bagplot(dat,na.rm=TRUE,factor=2.5,create.plot=TRUE,approx.limit=300,
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
    }


  ds<-maxsall-minsall; db<-maxball-minball
  slim<-c(minsall-0*ds/8,maxsall+ds/4); blim<-c(minball-0*db/4,maxball+0*db/8)
  } }
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
    bagplot(dat, ylab=paste0("Intercept"), xlab="1-slope (Amplitude)",cex=0.8,xlim=slim,ylim=blim,
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
    if(ncomp>minpoints){
      fit<-lm(y~x, na.action=na.exclude )
      cat("\n\nLINEAR FIT vs 1-slope for ALL DATA",main,
          "zero used is ", zero, "\nONLY intercept,error and intercept probability important\n")
      print(summary(fit))
      fit_coef<-coef(summary(fit))

      r2<-summary(fit)$r.squared

      cat(" a+z=constant=intersection point",fit_coef[2,"Estimate"]+zero," STD DEVa",fit_coef[2,"Std. Error"])
       Y<-fit_coef[2,"Estimate"]+zero
      cat(" intercept of fit = Group shift",fit_coef["(Intercept)","Estimate"]," STD DEV",fit_coef["(Intercept)","Std. Error"])

      cat("\n  N ",ncomp, " p(intercept differs from 0) ",fit_coef["(Intercept)","Pr(>|t|)"])

      cat("\nir",ir,"\n")
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
    }
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
        bagplot(dat,na.rm=TRUE,factor=2.5,create.plot=TRUE,approx.limit=300,
                show.outlier=TRUE,show.looppoints=TRUE,
                show.bagpoints=FALSE,dkmethod=2,
                show.whiskers=FALSE,show.loophull=TRUE,
                show.baghull=TRUE,verbose=FALSE,

                col.looppoints=colourpt[j],
                col.loophull=transgrey,
                col.baghull=vcolours[j],
                col.bagpoints="black",

                ylab=paste0("Intercept"), xlab="1-slope (-Amplitude relative to Ref)",
                main=paste("Reference\n",colnames(slope)[refindex],"zero time=",round(zero,digits=2),
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
    bagplot(dat,na.rm=TRUE,factor=2.5,create.plot=TRUE,approx.limit=300,
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
  return(bestintersectname )
}
