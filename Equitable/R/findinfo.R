findinfo <-
function(Tdave,printmax=FALSE,numb=NULL,ylim=NULL,slim=NULL,blim=NULL,main=" ",info=" ",extranames=NULL){
  eventintersect(Tdave,printmax=printmax,ylim=ylim)    # ,extranames=extranames
  if(is.null(ylim)){
    # ylim<- c(mean(c(Tdave$smat),na.rm = TRUE)-4*sd(c(Tdave$smat),na.rm = TRUE),
    #                          mean(c(Tdave$smat),na.rm = TRUE)+4*sd(c(Tdave$smat),na.rm = TRUE))
    maxsall<-max(c(Tdave$smat),na.rm = TRUE)
    minsall<-min(c(Tdave$smat),na.rm = TRUE)
    ds<-maxsall-minsall
    ylim<-c(minsall-ds*3/8,maxsall+ds*3/8)
  }
  if(is.null(blim)){
    # blim<- c(mean(c(Tdave$E.b),na.rm = TRUE)-4*sd(c(Tdave$E.b),na.rm = TRUE),
    #                          mean(c(Tdave$E.b),na.rm = TRUE)+4*sd(c(Tdave$E.b),na.rm = TRUE))
    maxsall<-max(c(Tdave$E.b),na.rm = TRUE)
    minsall<-min(c(Tdave$E.b),na.rm = TRUE)
    ds<-maxsall-minsall
    blim<-c(minsall-ds/8,maxsall+ds/4)
  }
  if(is.null(slim)){
    # slim<- c(mean(c(Tdave$E.s),na.rm = TRUE)-2*sd(c(Tdave$E.s),na.rm = TRUE),
    #                          mean(c(Tdave$E.s),na.rm = TRUE)+2*sd(c(Tdave$E.s),na.rm = TRUE))
    maxsall<-max(c(Tdave$E.s),na.rm = TRUE)
    minsall<-min(c(Tdave$E.s),na.rm = TRUE)
    ds<-maxsall-minsall
    slim<-c(minsall-ds/8,maxsall+ds/4)
  }

  #  c(mean(c(Tdave$smat),na.rm = TRUE)-4*sd(c(Tdave$smat),na.rm = TRUE),mean(c(Tdave$smat),na.rm = TRUE)+4*sd(c(Tdave$smat),na.rm = TRUE))
  # ylim<- c(mean(c(Tdave$smat),na.rm = TRUE)-4*sd(c(Tdave$smat),na.rm = TRUE),
  #          mean(c(Tdave$smat),na.rm = TRUE)+4*sd(c(Tdave$smat),na.rm = TRUE))

  op =
    par(mfrow =  c(1,1), mar = c(8,4.5, 4.5,4))
  s<-Tdave$E.s
  b<-Tdave$E.b
  r2<-Tdave$l.s.r2
  p<-1-Tdave$l.s.pslope

  slopefrom1<-0.1
  nsig<-2.0

  YM<-findYM(Tdave,nsig=nsig,slopefrom1=slopefrom1) #slopefrom1 restricts finding of Y to the use of slopes away from s-1>slopefrom
  Y<-YM$Y
  M<-YM$M
  YandM<-Y
  YandM[is.na(Y)]<-M[is.na(Y)]
  rY<-YM$rY
  rM<-YM$rM
  length(Y[!is.na(Y)])
  if(sd(YandM[!is.na(YandM)],na.rm=TRUE)>0){   #sd(M[!is.na(M)],na.rm=TRUE)
    imagenan(YandM,main="Intersection/parallel matrix Y and M")    #length(YandM[!is.na(YandM)])/length(YandM) length(YandM[!is.na(Yan)])/length(YandM)
  } else cat("\n Yand M matrix has no variation")
  if(length(M[!is.na(M)])>1){
    if(sd(YandM[!is.na(YandM)],na.rm=TRUE)>0){
      imagenan(M,main="Parallel matrix M ") # plot(diag(M))
    } else cat("\n M matrix has no variation")
  }
  if(length(Y[!is.na(Y)])>1){
    imagenan(Y,main="Intersection matrix Y ")
    diag(M)<-NA
    fi<-summary(c(Y),na.rm=TRUE)   #summary(c(Y),na.rm=TRUE)  fivenum(Y,na.rm=TRUE)
    cat("\nY and M criteria: Number of std. dev. away from 1 is ",nsig, "and slope must be ",slopefrom1,
        " away from one to be considered an intersection of lines\nOtherwise it could be considered parallel to the reference case (M type)")
    # cat("\nParallel(slope has error enough to be 1) ratio M(notNA) to whole matrix M  ",length(M[!is.na(M)])/length(M))
    # cat("\n\nLines differ and intersect: ratio Y(notNA) to whole matrix Y  ",length(Y[!is.na(Y)])/length(Y))
    # cat("\n ratio( Y(notNA)   + M(notNA) ) to whole matrix ",(length(M[!is.na(M)])+length(Y[!is.na(Y)]))/length(Y))
    # cat("\nEssentially Parallel (to within",slopefrom1,") but slope differs slightly from 1: ratio( YandM(NA) to whole matrix ",(length(YandM[is.na(YandM)]))/length(YandM))
    # cat("\n\nTotal Parallel: M and to within",slopefrom1,") ratio( M and YandM(NA) to whole matrix ",
    #     (length(M[!is.na(M)])+length(YandM[is.na(YandM)]))/length(YandM))
    # cat("\n\n Could be 1 ",length(M[!is.na(M)])/length(M)," Close to parallel ",(length(YandM[is.na(YandM)]))/length(YandM) ,
    #      " lines intersect ",length(Y[!is.na(Y)])/length(Y),
    #     " Total approximately Parallel: ",(length(M[!is.na(M)])+length(YandM[is.na(YandM)]))/length(YandM),"\n\n")
    sNAlength<-length(which(!is.na(Tdave$E.s)))   #length(which(Tdave$E.s==1))
    sNAlength<-sNAlength-ncol(Tdave$E.s)
    #no M values when exact so difference between sNAlength and nonNAlength is length of well defined inside

    #(sNAlength-nonNAlength)/sNAlength
    nonNAlength<-length(YandM[!is.na(YandM)])  #length of matrix for which slopes are outside bounds 1/1.1 and 1.1
    nonNAlength<-nonNAlength-ncol(Tdave$E.s)
    voutside<-Tdave$E.s ; voutside[which(Tdave$E.s<1.1 & Tdave$E.s>1/1.1)]<-NA   ;
    #imagenan(voutside);
    length_within<-length(voutside[which(is.na(voutside))])/sNAlength
    #imagenan(M);
    lengthM<-(length(which(!is.na(M))))/sNAlength  #lengthindise<-lengthM-lengthM_outside
    Moutside<-M ; Moutside[ is.na(voutside)]<-NA   ;
    #imagenan(Moutside);
    lengthM_outside<-length(which(!is.na(Moutside)))/sNAlength
    Minside<-M ; Minside[ !is.na(voutside)]<-NA   ;
    #imagenan(Minside);
    lengthM_inside<-length(which(!is.na(Minside)))/sNAlength
    #imagenan(Y);
    length(which(!is.na(Y)))/sNAlength
    allM<-length(M[!is.na(M)])/sNAlength
    #voutside[which(Tdave$E.s<1.1 & Tdave$E.s>1/1.1)]

    nsig=2
    #sss<-Tdave$E.s
    sss<-matrix(rep(1:length(Tdave$E.s)),nrow=nrow(Tdave$E.s),ncol=ncol(Tdave$E.s))
    clim<-c(1,length(Tdave$E.s))
    diag(sss)<-NA

    #sss[is.na(Tdave$E.s)]<-(-5)
    sss[is.na(Tdave$E.s)]<-NA
    #imagenan(sss)
    sl<-Tdave$E.s
    sl[is.na(sss)]<-NA
    imagenan(sl,main="Slopes")
    lengthNA<-length(which(!is.na(sss)))
    s_in<-s_out<-sss
    # sss[which(Tdave$E.s<1.1 & Tdave$E.s<1/1.1)]<-NA;length(which(!is.na(sss)))/; imagenan(sss)
    s_in[(Tdave$E.s>1.1 | Tdave$E.s<1/1.1)]<-NA  #sss[which(Tdave$E.s<1.1 & Tdave$E.s>1/1.1)]<-NA
    #imagenan(s_in)

    s_out[which(Tdave$E.s<=1.1 & Tdave$E.s>=(1/1.1))]<-NA
    imagenan(s_out,main="all individuals outside bounds")
    imagenan(s_in,main="all individuals inside bounds")   # s_in[,150]  Tdave$E.s[,150]
    #imagenan(s_out,main="outer",outside.below.color='red',zlim=clim)
    length_inside<-length(which(!is.na(s_in)))
    length_outside<-length(which(!is.na(s_out)))
    #imagenan(sss)
    #START HWERE
    sse<-nsig*Tdave$E.sd1/sqrt(Tdave$E.sN)
    slopetrue<-Tdave$E.s
    strue<-sss
    strue[which(abs(Tdave$E.s-1)-sse<0)]<-NA
    slopetrue[which(abs(Tdave$E.s-1)-sse<0)]<-NA
    imagenan(slopetrue,main="Well-defined Slopes")


    defined_within<-length(which(!is.na(strue) &!is.na(s_in) ))
    parallel_within<-length(which(is.na(strue) &!is.na(s_in) ))
    defined_within_to_inside<-defined_within/length_inside
    parallel_within_to_inside<-parallel_within/length_inside

    defined_within_to_all<-defined_within/lengthNA
    parallel_within_to_all<-parallel_within/lengthNA

    cat("\n\nPORTION of WITHIN REGION(similar slopes): Well-defined",defined_within_to_inside," (nearly) parallel",parallel_within_to_inside)
    cat("\nRATIO OF WITHIN (similar slopes) To ALL: Well-defined",defined_within_to_all, " (nearly) parallel",parallel_within_to_all)

    defined_outside<-length(which(!is.na(strue) &!is.na(s_out) ))
    parallel_outside<-length(which(is.na(strue) &!is.na(s_out) ))
    defined_outside_to_outside<-defined_outside/length_outside
    parallel_outside_to_outside<-parallel_outside/length_outside
    defined_outside_to_all<-defined_outside/lengthNA
    parallel_outside_to_all<-parallel_outside/lengthNA
    alldefined_to_all<-(defined_outside+defined_within)/lengthNA
    allpoor_to_all<-(parallel_within+parallel_outside)/lengthNA

    cat("\n\nPORTION of OUTSIDE REGION(different slopes): Well-defined",defined_outside_to_outside," (Could be) parallel",parallel_outside_to_outside)
    cat("\n  RATIO OF OUTSIDE (different slopes) To ALL: Well-defined ",defined_outside_to_all,    " (Could be) parallel",parallel_outside_to_all)
    cat("\nsum of all regions ",parallel_outside_to_all+defined_outside_to_all+parallel_within_to_all+defined_within_to_all)

    cat("\n\nAll well-defined slopes to ALL (Very likely NOT parallel (NOT)",alldefined_to_all )
    cat("  \nAll poorly-defined slopes to ALL (Could be  parallel          ",allpoor_to_all )
    cat("\nWell-defined, poorly defined",alldefined_to_all,allpoor_to_all)

    outside_to_all<-length_outside/lengthNA
    inside_to_all<-length_inside/lengthNA
    cat("\n\nRatio of WITHIN slopes to whole (notNA)slope matrix (ABOUT PARALLEL: whether welldefined or not)  ",inside_to_all)
    cat("  \nRatio of  OUTSIDE slopes to whole (notNA)slope matrix          (DIFFERENT)                        ",outside_to_all)
    cat("\nportion of non NA slopes to whole slope matrix ",lengthNA/length(sss),"\n")

    #length_inside+length_outside
    # Youtside<-Y ; Youtside[ is.na(voutside)]<-NA   ; imagenan(Youtside); lengthY_outside<-length(which(!is.na(Youtside)))/sNAlength
    # Yinside<-Y ; Yinside[ !is.na(voutside)]<-NA   ; imagenan(Yinside); lengthY_inside<-length(which(!is.na(Yinside)))/sNAlength
    #no Y values within bounds
    #sNAlength-nonNAlength # slope defined but Y,M not
    #need fraction intersecting from only ouside bound group , only inside bound group
    #need fraction parallel from only ouside bound group , only inside bound group
    # cat("\n\nCould be parallel due to error (includes all of matrix)",allM,
    #     "\nCould be parallel due to error (Within bounds)",lengthM_inside,
    #     "\nCould be parallel due to error (outside bounds)",lengthM_outside,
    #     "\nlines intersect ( only region outside bounds)",Youtside,
    #     "\nlines well defined (region within bounds)",defined_within,
    #      "\nTotal well defined (including well defined inbound slopes): ",
    #      Youtside+defined_within,
    #     "\n all",Youtside+defined_within+allM)            #+length(sss[which(is.na(sss))])/sNAlength
    #
    # cat("\n\nCould be parallel: WITHIN BOUND REGION RATIO",lengthM_inside*sNAlength/(length_inside),
    #     "\nCould be parallel: OUTSIDE BOUND REGION RATIO",lengthM_outside*sNAlength/length_outside,
    #     "\nlines intersect: OUTSIDE BOUND REGION RATIO",Youtside*sNAlength/(nonNAlength-ncol(Tdave$E.s)+1),
    #     "\nlines well defined: WITHIN BOUND REGION RATIO",defined_within*sNAlength/(sNAlength-nonNAlength-1+ncol(Tdave$E.s))
    #     )
    #  "\nTotal approximately Parallel (assuming all within bounds slopes are parallel): ",
    #  (length(M[!is.na(M)])+length(YandM[is.na(YandM)]))/nonNAlength,"\n",
    #  "\nTotal well defined (including well defined inbound slopes): ",
    #  (length(Y[!is.na(Y)])+length(YandM)-nonNAlength )/length(!is.na(Tdave$E.s)),
    #  "\nTotal Ambiguous (including ambiguous inbound slopes): ",
    #  "Within bounds slopes ",length_within/length(!is.na(Tdave$E.s)),
    #  "M outside of bounds ",
    # ( length(M[!is.na(M)])-length_within )/length(!is.na(Tdave$E.s)),
    #  "\n\n")

    cat("\n Approximately 10%  may or may not be parallel when noise is large  \n")

    cat("\n Y summary  Median value to compare with mean values of events\n")
    print(summary(c(Y),na.rm=TRUE))
    cat("\n rY summary  Median value to compare with mean values of events\n")
    fir<-summary(c(rY),na.rm=TRUE)
    print(fir)
    if(!is.na(fir["1st Qu."])) cat("\n 1st Qu. intersection point is ",
                                   rownames(Tdave$ET.x)[round(fir["1st Qu."])],"1st Qu. Y ",fi["1st Qu."])
    if(!is.na(fir["Median"])){
      cat("\n\n most likely intersection point is ",
          rownames(Tdave$ET.x)[round(fir["Median"])]," Median Y ",fi["Median"],"\n\n")
      rinter<- round(fir["Median"])

    } else rinter<-NA
    if(!is.na(fir["3rd Qu."])) cat("\n 3rd Qu. intersection point is ",
                                   rownames(Tdave$ET.x)[round(fir["3rd Qu."])]," 3rd Qu. Y ",fi["3rd Qu."])

    #hist(Y,main=paste("Intersection :Zero= ",round(Tdave$l.s.zero,digits=0)))


    rM<-rowMeans(Tdave$ET.x,na.rm=TRUE)
    if(printmax){
      cat("\n\nmean values of events \n",names(rM),"\n")
      cat(rM)
    }

    if(is.null(numb))nc<-1:ncol(Tdave$smat) else{
      inc<-round(ncol(Tdave$smat)/numb)  # inc<-2
      if(inc<1)inc<-1
      nc<-seq(1,ncol(Tdave$smat),by=inc)
    }

    sm<-colMeans(Tdave$E.s,na.rm=TRUE)
    #plot(sm-1)
    sig<-sd(sm,na.rm=TRUE)   ;
    lowval<-quantile(sm, 0.10,na.rm=TRUE)

    lll<-which(abs(sm)<=lowval)
    if(length(lll)>0){
      #plot(sm[lll]); lines(0:50,rep(lowval,51)); lines(0:50,rep(highval,51))
      if(length(lll)>1){
        pm<-colMeans(Tdave$l.s.pslope[,lll],na.rm=TRUE) #find all p averages except Row_ave
        refer<-names(pm)[which(pm==min(pm,na.rm=TRUE))[1]]
      }  else {
        pm<-mean(Tdave$l.s.pslope[,lll],na.rm=TRUE);refer<-lll
      }
      if(!is.na(refer)){
        xvsrefplot(Td=Tdave,cgroup=nc,ylim=ylim,ref=refer,br=paste0(info, "\nComparison (min p for bottom 10% of slopes)"),extranames=extranames)
        main<-paste("\n",info,"\nReference (min p for bottom 10% of slopes) ",round(min(pm,na.rm=TRUE),digits=4),"\n",refer)
        plot_hist(Tdave,refer=refer,main=main,slim=slim,blim=blim)
      }
    } else {cat("\nno bottom values (<10%) found\n")}

    highval<-quantile(sm, 0.9,na.rm=TRUE)   # highval<-quantile(sm, 0.99,na.rm=TRUE)   ; which(abs(sm)>=highval)
    lll<-which(abs(sm)>=highval)
    if(length(lll)>0){
      if(length(lll)>1){
        pm<-colMeans(Tdave$l.s.pslope[,lll],na.rm=TRUE) #find all p averages except Row_ave
        refer<-names(pm)[which(pm==min(pm,na.rm=TRUE))[1]]
      }  else {
        pm<-mean(Tdave$l.s.pslope[,lll],na.rm=TRUE);refer<-lll
      }
      if(!is.na(refer)){
        xvsrefplot(Td=Tdave,cgroup=nc,ylim=ylim,ref=refer,br=paste0(info, "\nComparison (min p for top 10% of slopes)"),extranames=extranames)
        main<-paste("\n",info,"\nReference (min p for top 10% of slopes) ",round(min(pm,na.rm=TRUE),digits=4),"\n",refer)
        plot_hist(Tdave,refer=refer,main=main,slim=slim,blim=blim)
      }
    } else {cat("\nno top values (>90%) found\n")}
    #which(colnames(Tdave$E.s)==refer)


    la<-which(colnames(Tdave$l.s.pslope)=="Row_Ave")
    if(length(la)!=0){pm<-colMeans(Tdave$l.s.pslope[,-la],na.rm=TRUE) #find all p averages except Row_ave
    }  else pm<-colMeans(Tdave$l.s.pslope,na.rm=TRUE)
    refer<-which(pm==min(pm,na.rm=TRUE))[1]
    if(!is.na(refer)){
      xvsrefplot(Td=Tdave,cgroup=nc,ylim=ylim,ref=refer,br=paste0(info, "\nComparison (min p for all individuals)"),extranames=extranames)
      main<-paste("\n",info,"\nReference (min p for all individuals) ",round(min(pm,na.rm=TRUE),digits=4),"\n",colnames(Tdave$l.s.pslope)[refer])
      plot_hist(Tdave,refer=refer,main=main,slim=slim,blim=blim)
    }



    refer<-which(colnames(Tdave$l.s.pslope)=="Row_Ave")[1]
    if(!is.na(refer)){

      xvsrefplot(Td=Tdave,cgroup=nc,ylim=ylim,ref=refer,br=paste0(" Equitable Profiles compared to Average Profile"))
      main<-paste("Average Reference\n",colnames(Tdave$l.s.pslope)[refer])
      plot_hist(Tdave,refer=refer,slim=slim,blim=blim,main=main)
    }

    #
    main<-paste("Entire Matrix",main)
    cat("\n",main,"\n")
    plot_hist(Tdave,main=main,slim=slim,blim=blim)




    rownum<-rownames(Tdave$ET.x)[rinter]
    hist(Y,breaks=50,main=paste("Intersections :Zero= ",round(Tdave$l.s.zero,digits=0),"\nMost Likely Event Intersection event",rownames(Tdave$ET.x)[rinter]))
    if(!is.na(fi["Median"]))lines(rep(fi["Median"],1000),0:999,lwd=4)
    if(!is.na(fi["1st Qu."]))lines(rep(fi["1st Qu."],1000),0:999,lwd=3,lty=2)
    if(!is.na(fi["3rd Qu."]))lines(rep(fi["3rd Qu."],1000),0:999,lwd=3,lty=2)
    #if(rinter<length(rM))lines(rep(rM[rinter+1],1000),0:999,lwd=3,lty=2)
    if(2<length(which(!is.na(rY)))){
      hist(rY,breaks=50,main=paste("Intersection Index:Zero= ",round(Tdave$l.s.zero,digits=0),"\nMost Likely Event Intersection",rownames(Tdave$ET.x)[rinter]))
      if(!is.na(fir["Median"]))lines(rep(fir["Median"],1000),0:999,lwd=4)
      if(!is.na(fir["1st Qu."]))lines(rep(fir["1st Qu."],1000),0:999,lwd=3,lty=2)
      if(!is.na(fir["3rd Qu."]))lines(rep(fir["3rd Qu."],1000),0:999,lwd=3,lty=2)
      #if(rinter<length(rM))lines(rep(rM[rinter+1],1000),0:999,lwd=3,lty=2)
    }
    if(!is.na(rinter)&& !is.na(fi["1st Qu."]) && !is.na(fi["3rd Qu."])){
      d<-Tdave$ET.x
      minval<-fi["Median"]-abs(fi["Median"]-fi["1st Qu."])/4
      maxval<-fi["Median"]+abs(fi["Median"]-fi["3rd Qu."])/4
      d[minval<d & d<maxval]<-NA ; #d1[-1.01<d & d<1.01]<-NA
      imagenan(d);
      title(main=paste("Equitable Transform:Region (+-1/4Sigma)\nAround median intersection= ",round(fi["Median"],digits=1)," grey"),cex.main=0.8)
    }
  } else { cat("\n\n\n Intersection matrix empty\n\n")}

  I<-Tdave$ET.x
  #Td6_9<-Tdave
  #imagenan(YandM,main="YandM Matrix")   #fivenum(1-c(Tdave$l.s.pslope),na.rm=TRUE)  summary(fivenum(1-c(Tdave$l.s.pslope),na.rm=TRUE))
  hist(1-p,xlim=c(0.00,0.15),breaks=50,main="Histogram\nProbability for No Correlation Matrix Values")



  cat("\n\nThreshold Matrix probability value for quantile percentage of matrix values\n")

  print(quantile(1-p, 0.80,na.rm=TRUE) )
  print(quantile(1-p, 0.85,na.rm=TRUE) )
  print(quantile(1-p, 0.90,na.rm=TRUE) )
  print(quantile(1-p, 0.95,na.rm=TRUE) )
  cat("\nquartile summary for matrix of Prob. for no correlation p\n")
  print(summary(c(1-p)))#print(summary(c(p)))

  cat("\n")
  # nt<-which(min(c(p),na.rm=TRUE)==c(p)) #   plot(c(p))  nt%%nrow(p) nt%/%nrow(p)
  # #p[(nt%/%nrow(p)+1),nt%%nrow(p)]
  # rmin<-(nt[1]%/%nrow(p)+1)
  # cmin<-nt[1]%%nrow(p)
  # p[p==min(c(p),na.rm=TRUE)]

  for (r in 1:nrow(p))for (cx in 1:ncol(p))if(!is.na(p[r,cx]))if(min(c(p),na.rm=TRUE)==p[r,cx]){rmin<-r;cmin<-cx }

  cat("\nWorst fit at p=",1-p[rmin,cmin],"\n",colnames(p)[rmin])
  cat("\n",colnames(p)[cmin],"\n")
  plot(Tdave$smat[,cmin],Tdave$smat[,rmin],
       xlab=colnames(p)[cmin],ylab=colnames(p)[rmin])
  title(main=paste0("Worst fit:\nRow Individual ",colnames(p)[rmin],"\nvs Col Individual ",colnames(p)[cmin]),cex.main=0.8)
  #print(Tdave$smat[,rmin])
  return(rownum[1])
}
