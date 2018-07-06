\name{findinfo}
\alias{findinfo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
get information on intersections
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
findinfo(Tdave, printmax = FALSE, numb = NULL, ylim = NULL, slim = NULL, blim = NULL, main = " ", info = " ", extranames = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Tdave}{
%%     ~~Describe \code{Tdave} here~~
}
  \item{printmax}{
%%     ~~Describe \code{printmax} here~~
}
  \item{numb}{
%%     ~~Describe \code{numb} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
}
  \item{slim}{
%%     ~~Describe \code{slim} here~~
}
  \item{blim}{
%%     ~~Describe \code{blim} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
}
  \item{info}{
%%     ~~Describe \code{info} here~~
}
  \item{extranames}{
%%     ~~Describe \code{extranames} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (Tdave, printmax = FALSE, numb = NULL, ylim = NULL, 
    slim = NULL, blim = NULL, main = " ", info = " ", extranames = NULL) 
{
    eventintersect(Tdave, printmax = printmax, ylim = ylim)
    if (is.null(ylim)) {
        maxsall <- max(c(Tdave$smat), na.rm = TRUE)
        minsall <- min(c(Tdave$smat), na.rm = TRUE)
        ds <- maxsall - minsall
        ylim <- c(minsall - ds * 3/8, maxsall + ds * 3/8)
    }
    if (is.null(blim)) {
        maxsall <- max(c(Tdave$E.b), na.rm = TRUE)
        minsall <- min(c(Tdave$E.b), na.rm = TRUE)
        ds <- maxsall - minsall
        blim <- c(minsall - ds/8, maxsall + ds/4)
    }
    if (is.null(slim)) {
        maxsall <- max(c(Tdave$E.s), na.rm = TRUE)
        minsall <- min(c(Tdave$E.s), na.rm = TRUE)
        ds <- maxsall - minsall
        slim <- c(minsall - ds/8, maxsall + ds/4)
    }
    op = par(mfrow = c(1, 1), mar = c(8, 4.5, 4.5, 4))
    s <- Tdave$E.s
    b <- Tdave$E.b
    r2 <- Tdave$l.s.r2
    p <- 1 - Tdave$l.s.pslope
    slopefrom1 <- 0.1
    nsig <- 2
    YM <- findYM(Tdave, nsig = nsig, slopefrom1 = slopefrom1)
    Y <- YM$Y
    M <- YM$M
    YandM <- Y
    YandM[is.na(Y)] <- M[is.na(Y)]
    rY <- YM$rY
    rM <- YM$rM
    length(Y[!is.na(Y)])
    if (sd(YandM[!is.na(YandM)], na.rm = TRUE) > 0) {
        imagenan(YandM, main = "Intersection/parallel matrix Y and M")
    }
    else cat("\n Yand M matrix has no variation")
    if (length(M[!is.na(M)]) > 1) {
        if (sd(YandM[!is.na(YandM)], na.rm = TRUE) > 0) {
            imagenan(M, main = "Parallel matrix M ")
        }
        else cat("\n M matrix has no variation")
    }
    if (length(Y[!is.na(Y)]) > 1) {
        imagenan(Y, main = "Intersection matrix Y ")
        diag(M) <- NA
        fi <- summary(c(Y), na.rm = TRUE)
        cat("\nY and M criteria: Number of std. dev. away from 1 is ", 
            nsig, "and slope must be ", slopefrom1, " away from one to be considered an intersection of lines\nOtherwise it could be considered parallel to the reference case (M type)")
        sNAlength <- length(which(!is.na(Tdave$E.s)))
        sNAlength <- sNAlength - ncol(Tdave$E.s)
        nonNAlength <- length(YandM[!is.na(YandM)])
        nonNAlength <- nonNAlength - ncol(Tdave$E.s)
        voutside <- Tdave$E.s
        voutside[which(Tdave$E.s < 1.1 & Tdave$E.s > 1/1.1)] <- NA
        length_within <- length(voutside[which(is.na(voutside))])/sNAlength
        lengthM <- (length(which(!is.na(M))))/sNAlength
        Moutside <- M
        Moutside[is.na(voutside)] <- NA
        lengthM_outside <- length(which(!is.na(Moutside)))/sNAlength
        Minside <- M
        Minside[!is.na(voutside)] <- NA
        lengthM_inside <- length(which(!is.na(Minside)))/sNAlength
        length(which(!is.na(Y)))/sNAlength
        allM <- length(M[!is.na(M)])/sNAlength
        nsig = 2
        sss <- matrix(rep(1:length(Tdave$E.s)), nrow = nrow(Tdave$E.s), 
            ncol = ncol(Tdave$E.s))
        clim <- c(1, length(Tdave$E.s))
        diag(sss) <- NA
        sss[is.na(Tdave$E.s)] <- NA
        sl <- Tdave$E.s
        sl[is.na(sss)] <- NA
        imagenan(sl, main = "Slopes")
        lengthNA <- length(which(!is.na(sss)))
        s_in <- s_out <- sss
        s_in[(Tdave$E.s > 1.1 | Tdave$E.s < 1/1.1)] <- NA
        s_out[which(Tdave$E.s <= 1.1 & Tdave$E.s >= (1/1.1))] <- NA
        imagenan(s_out, main = "all individuals outside bounds")
        imagenan(s_in, main = "all individuals inside bounds")
        length_inside <- length(which(!is.na(s_in)))
        length_outside <- length(which(!is.na(s_out)))
        sse <- nsig * Tdave$E.sd1/sqrt(Tdave$E.sN)
        slopetrue <- Tdave$E.s
        strue <- sss
        strue[which(abs(Tdave$E.s - 1) - sse < 0)] <- NA
        slopetrue[which(abs(Tdave$E.s - 1) - sse < 0)] <- NA
        imagenan(slopetrue, main = "Well-defined Slopes")
        defined_within <- length(which(!is.na(strue) & !is.na(s_in)))
        parallel_within <- length(which(is.na(strue) & !is.na(s_in)))
        defined_within_to_inside <- defined_within/length_inside
        parallel_within_to_inside <- parallel_within/length_inside
        defined_within_to_all <- defined_within/lengthNA
        parallel_within_to_all <- parallel_within/lengthNA
        cat("\n\nPORTION of WITHIN REGION(similar slopes): Well-defined", 
            defined_within_to_inside, " (nearly) parallel", parallel_within_to_inside)
        cat("\nRATIO OF WITHIN (similar slopes) To ALL: Well-defined", 
            defined_within_to_all, " (nearly) parallel", parallel_within_to_all)
        defined_outside <- length(which(!is.na(strue) & !is.na(s_out)))
        parallel_outside <- length(which(is.na(strue) & !is.na(s_out)))
        defined_outside_to_outside <- defined_outside/length_outside
        parallel_outside_to_outside <- parallel_outside/length_outside
        defined_outside_to_all <- defined_outside/lengthNA
        parallel_outside_to_all <- parallel_outside/lengthNA
        alldefined_to_all <- (defined_outside + defined_within)/lengthNA
        allpoor_to_all <- (parallel_within + parallel_outside)/lengthNA
        cat("\n\nPORTION of OUTSIDE REGION(different slopes): Well-defined", 
            defined_outside_to_outside, " (Could be) parallel", 
            parallel_outside_to_outside)
        cat("\n  RATIO OF OUTSIDE (different slopes) To ALL: Well-defined ", 
            defined_outside_to_all, " (Could be) parallel", parallel_outside_to_all)
        cat("\nsum of all regions ", parallel_outside_to_all + 
            defined_outside_to_all + parallel_within_to_all + 
            defined_within_to_all)
        cat("\n\nAll well-defined slopes to ALL (Very likely NOT parallel (NOT)", 
            alldefined_to_all)
        cat("  \nAll poorly-defined slopes to ALL (Could be  parallel          ", 
            allpoor_to_all)
        cat("\nWell-defined, poorly defined", alldefined_to_all, 
            allpoor_to_all)
        outside_to_all <- length_outside/lengthNA
        inside_to_all <- length_inside/lengthNA
        cat("\n\nRatio of WITHIN slopes to whole (notNA)slope matrix (ABOUT PARALLEL: whether welldefined or not)  ", 
            inside_to_all)
        cat("  \nRatio of  OUTSIDE slopes to whole (notNA)slope matrix          (DIFFERENT)                        ", 
            outside_to_all)
        cat("\nportion of non NA slopes to whole slope matrix ", 
            lengthNA/length(sss), "\n")
        cat("\n Approximately 10\%  may or may not be parallel when noise is large  \n")
        cat("\n Y summary  Median value to compare with mean values of events\n")
        print(summary(c(Y), na.rm = TRUE))
        cat("\n rY summary  Median value to compare with mean values of events\n")
        fir <- summary(c(rY), na.rm = TRUE)
        print(fir)
        if (!is.na(fir["1st Qu."])) 
            cat("\n 1st Qu. intersection point is ", rownames(Tdave$ET.x)[round(fir["1st Qu."])], 
                "1st Qu. Y ", fi["1st Qu."])
        if (!is.na(fir["Median"])) {
            cat("\n\n most likely intersection point is ", rownames(Tdave$ET.x)[round(fir["Median"])], 
                " Median Y ", fi["Median"], "\n\n")
            rinter <- round(fir["Median"])
        }
        else rinter <- NA
        if (!is.na(fir["3rd Qu."])) 
            cat("\n 3rd Qu. intersection point is ", rownames(Tdave$ET.x)[round(fir["3rd Qu."])], 
                " 3rd Qu. Y ", fi["3rd Qu."])
        rM <- rowMeans(Tdave$ET.x, na.rm = TRUE)
        if (printmax) {
            cat("\n\nmean values of events \n", names(rM), "\n")
            cat(rM)
        }
        if (is.null(numb)) 
            nc <- 1:ncol(Tdave$smat)
        else {
            inc <- round(ncol(Tdave$smat)/numb)
            if (inc < 1) 
                inc <- 1
            nc <- seq(1, ncol(Tdave$smat), by = inc)
        }
        sm <- colMeans(Tdave$E.s, na.rm = TRUE)
        sig <- sd(sm, na.rm = TRUE)
        lowval <- quantile(sm, 0.1, na.rm = TRUE)
        lll <- which(abs(sm) <= lowval)
        if (length(lll) > 0) {
            if (length(lll) > 1) {
                pm <- colMeans(Tdave$l.s.pslope[, lll], na.rm = TRUE)
                refer <- names(pm)[which(pm == min(pm, na.rm = TRUE))[1]]
            }
            else {
                pm <- mean(Tdave$l.s.pslope[, lll], na.rm = TRUE)
                refer <- lll
            }
            if (!is.na(refer)) {
                xvsrefplot(Td = Tdave, cgroup = nc, ylim = ylim, 
                  ref = refer, br = paste0(info, "\nComparison (min p for bottom 10\% of slopes)"), 
                  extranames = extranames)
                main <- paste("\n", info, "\nReference (min p for bottom 10\% of slopes) ", 
                  round(min(pm, na.rm = TRUE), digits = 4), "\n", 
                  refer)
                plot_hist(Tdave, refer = refer, main = main, 
                  slim = slim, blim = blim)
            }
        }
        else {
            cat("\nno bottom values (<10\%) found\n")
        }
        highval <- quantile(sm, 0.9, na.rm = TRUE)
        lll <- which(abs(sm) >= highval)
        if (length(lll) > 0) {
            if (length(lll) > 1) {
                pm <- colMeans(Tdave$l.s.pslope[, lll], na.rm = TRUE)
                refer <- names(pm)[which(pm == min(pm, na.rm = TRUE))[1]]
            }
            else {
                pm <- mean(Tdave$l.s.pslope[, lll], na.rm = TRUE)
                refer <- lll
            }
            if (!is.na(refer)) {
                xvsrefplot(Td = Tdave, cgroup = nc, ylim = ylim, 
                  ref = refer, br = paste0(info, "\nComparison (min p for top 10\% of slopes)"), 
                  extranames = extranames)
                main <- paste("\n", info, "\nReference (min p for top 10\% of slopes) ", 
                  round(min(pm, na.rm = TRUE), digits = 4), "\n", 
                  refer)
                plot_hist(Tdave, refer = refer, main = main, 
                  slim = slim, blim = blim)
            }
        }
        else {
            cat("\nno top values (>90\%) found\n")
        }
        la <- which(colnames(Tdave$l.s.pslope) == "Row_Ave")
        if (length(la) != 0) {
            pm <- colMeans(Tdave$l.s.pslope[, -la], na.rm = TRUE)
        }
        else pm <- colMeans(Tdave$l.s.pslope, na.rm = TRUE)
        refer <- which(pm == min(pm, na.rm = TRUE))[1]
        if (!is.na(refer)) {
            xvsrefplot(Td = Tdave, cgroup = nc, ylim = ylim, 
                ref = refer, br = paste0(info, "\nComparison (min p for all individuals)"), 
                extranames = extranames)
            main <- paste("\n", info, "\nReference (min p for all individuals) ", 
                round(min(pm, na.rm = TRUE), digits = 4), "\n", 
                colnames(Tdave$l.s.pslope)[refer])
            plot_hist(Tdave, refer = refer, main = main, slim = slim, 
                blim = blim)
        }
        refer <- which(colnames(Tdave$l.s.pslope) == "Row_Ave")[1]
        if (!is.na(refer)) {
            xvsrefplot(Td = Tdave, cgroup = nc, ylim = ylim, 
                ref = refer, br = paste0(" Equitable Profiles compared to Average Profile"))
            main <- paste("Average Reference\n", colnames(Tdave$l.s.pslope)[refer])
            plot_hist(Tdave, refer = refer, slim = slim, blim = blim, 
                main = main)
        }
        main <- paste("Entire Matrix", main)
        cat("\n", main, "\n")
        plot_hist(Tdave, main = main, slim = slim, blim = blim)
        rownum <- rownames(Tdave$ET.x)[rinter]
        hist(Y, breaks = 50, main = paste("Intersections :Zero= ", 
            round(Tdave$l.s.zero, digits = 0), "\nMost Likely Event Intersection event", 
            rownames(Tdave$ET.x)[rinter]))
        if (!is.na(fi["Median"])) 
            lines(rep(fi["Median"], 1000), 0:999, lwd = 4)
        if (!is.na(fi["1st Qu."])) 
            lines(rep(fi["1st Qu."], 1000), 0:999, lwd = 3, lty = 2)
        if (!is.na(fi["3rd Qu."])) 
            lines(rep(fi["3rd Qu."], 1000), 0:999, lwd = 3, lty = 2)
        if (2 < length(which(!is.na(rY)))) {
            hist(rY, breaks = 50, main = paste("Intersection Index:Zero= ", 
                round(Tdave$l.s.zero, digits = 0), "\nMost Likely Event Intersection", 
                rownames(Tdave$ET.x)[rinter]))
            if (!is.na(fir["Median"])) 
                lines(rep(fir["Median"], 1000), 0:999, lwd = 4)
            if (!is.na(fir["1st Qu."])) 
                lines(rep(fir["1st Qu."], 1000), 0:999, lwd = 3, 
                  lty = 2)
            if (!is.na(fir["3rd Qu."])) 
                lines(rep(fir["3rd Qu."], 1000), 0:999, lwd = 3, 
                  lty = 2)
        }
        if (!is.na(rinter) && !is.na(fi["1st Qu."]) && !is.na(fi["3rd Qu."])) {
            d <- Tdave$ET.x
            minval <- fi["Median"] - abs(fi["Median"] - fi["1st Qu."])/4
            maxval <- fi["Median"] + abs(fi["Median"] - fi["3rd Qu."])/4
            d[minval < d & d < maxval] <- NA
            imagenan(d)
            title(main = paste("Equitable Transform:Region (+-1/4Sigma)\nAround median intersection= ", 
                round(fi["Median"], digits = 1), " grey"), cex.main = 0.8)
        }
    }
    else {
        cat("\n\n\n Intersection matrix empty\n\n")
    }
    I <- Tdave$ET.x
    hist(1 - p, xlim = c(0, 0.15), breaks = 50, main = "Histogram\nProbability for No Correlation Matrix Values")
    cat("\n\nThreshold Matrix probability value for quantile percentage of matrix values\n")
    print(quantile(1 - p, 0.8, na.rm = TRUE))
    print(quantile(1 - p, 0.85, na.rm = TRUE))
    print(quantile(1 - p, 0.9, na.rm = TRUE))
    print(quantile(1 - p, 0.95, na.rm = TRUE))
    cat("\nquartile summary for matrix of Prob. for no correlation p\n")
    print(summary(c(1 - p)))
    cat("\n")
    for (r in 1:nrow(p)) for (cx in 1:ncol(p)) if (!is.na(p[r, 
        cx])) 
        if (min(c(p), na.rm = TRUE) == p[r, cx]) {
            rmin <- r
            cmin <- cx
        }
    cat("\nWorst fit at p=", 1 - p[rmin, cmin], "\n", colnames(p)[rmin])
    cat("\n", colnames(p)[cmin], "\n")
    plot(Tdave$smat[, cmin], Tdave$smat[, rmin], xlab = colnames(p)[cmin], 
        ylab = colnames(p)[rmin])
    title(main = paste0("Worst fit:\nRow Individual ", colnames(p)[rmin], 
        "\nvs Col Individual ", colnames(p)[cmin]), cex.main = 0.8)
    return(rownum[1])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
