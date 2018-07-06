\name{a_b_bagplot}
\alias{a_b_bagplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
build a bagplot from 1-slope (1-a) and intercept b
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
a_b_bagplot(community.f = NULL, Td, refindex = "Row_Ave", xlim = NULL, ylim = NULL, main = " ", rownum = NULL, facname = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{community.f}{
%%     ~~Describe \code{community.f} here~~
}
  \item{Td}{
%%     ~~Describe \code{Td} here~~
}
  \item{refindex}{
%%     ~~Describe \code{refindex} here~~
}
  \item{xlim}{
%%     ~~Describe \code{xlim} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
}
  \item{rownum}{
%%     ~~Describe \code{rownum} here~~
}
  \item{facname}{
%%     ~~Describe \code{facname} here~~
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
function (community.f = NULL, Td, refindex = "Row_Ave", xlim = NULL, 
    ylim = NULL, main = " ", rownum = NULL, facname = NULL) 
{
    if (ncol(Td$E.b) < 60) {
        quant1 <- 0.33
        quant2 <- 0.67
    }
    if (ncol(Td$E.b) > 60 && nrow(Td$E.b) <= 150) {
        quant1 <- 0.1
        quant2 <- 0.9
    }
    if (ncol(Td$E.b) > 150) {
        quant1 <- 0.05
        quant2 <- 0.95
    }
    bestintersectname <- NULL
    minpoints <- 4
    lotscol <- colors()[c(24, 94, 26, 124, 633, 450, 453, 11, 
        68, 254, 257, 51, 630, 76, 142, 150, 653)]
    translevel <- 0.35
    transgrey <- add.alpha("grey", translevel)
    lotscol1 <- add.alpha(lotscol, translevel)
    if (!is.numeric(refindex)) {
        refindex <- which(colnames(Td$smat) == refindex)
    }
    op = par(mfrow = c(1, 1), mar = c(8, 4.5, 4.5, 4))
    if (is.null(rownum)) {
        if (nrow(Td$ET.x) > 20) 
            rinc <- ceiling((nrow(Td$ET.x))/(10))
        else rinc = 1
        ir <- seq(1, nrow(Td$ET.x), by = rinc)
        if (ir[length(ir)] != nrow(Td$ET.x)) 
            ir <- c(ir, nrow(Td$ET.x))
        zerolist <- Td$ET.x[ir, refindex]
        vcolours <- lotscol1[1:length(ir)]
        colourpt <- lotscol[1:length(ir)]
    }
    else {
        if (is.numeric(rownum)) 
            ir <- rownum
        else ir <- which(rownames(Td$ET.x) == rownum)
        zerolist <- Td$ET.x[rownum, refindex]
        vcolours <- "blue"
        colourpt <- "blue"
    }
    cat("\nir is ", ir, "\nrownames are ", rownames(Td$ET.x)[ir], 
        "\n")
    if (is.null(xlim)) {
        ff <- quantile(c(1 - Td$E.s[, refindex]), probs = c(0, 
            1), na.rm = TRUE)
        slim <- c(ff[1], ff[2] + (ff[2] - ff[1])/5)
    }
    else {
        slim <- xlim
    }
    if (is.null(ylim)) {
        ff <- quantile(c(Td$ET.x[, refindex]), probs = c(0, 1), 
            na.rm = TRUE)
        half <- (ff[2] - ff[1])/2
        blim <- c(-half, +half)
    }
    else {
        blim <- ylim
    }
    if (!is.null(community.f)) {
        levc <- length(levels(community.f))
        if (levc > 1) {
            cat("\nmain factor\n")
            print(summary(community.f))
            slope <- Td$E.s[, refindex]
            bcol <- lotscol[4:(length(levels(community.f)) + 
                3)]
            boxplot(slope ~ community.f, na.action = na.exclude, 
                col = bcol, lwd = 2, notch = FALSE, main = paste("zero=", 
                  round(Td$l.s.zero, digits = 1), "\n", facname, 
                  main), cex = 1, cex.axis = 0.7, ylab = "Slope", 
                cex.main = 0.8, xlab = paste(facname), cex.lab = 1.2)
            legend("topright", legend = levels(community.f), 
                fill = bcol)
            cat("\n\n\nSlope Linear model ", " with ", levels(community.f), 
                "\n\n")
            print(summary(lm(slope ~ community.f, na.action = na.exclude)))
            for (irow in ir) {
                zero <- Td$ET.x[irow, refindex]
                zname <- rownames(Td$ET.x)[irow]
                newb <- reviseb_witherror(rownum = irow, Td = Td, 
                  ref = refindex)
                intercept <- newb$E.b[, refindex]
                boxplot(intercept ~ community.f, na.action = na.exclude, 
                  col = bcol, lwd = 2, notch = FALSE, main = paste("zero=", 
                    round(zero, digits = 1), zname, "\n", facname, 
                    main), cex = 1, cex.axis = 0.7, ylab = "Intercept", 
                  cex.main = 0.8, xlab = paste(facname), cex.lab = 1.2)
                legend("topright", legend = levels(community.f), 
                  fill = bcol)
                cat("\n\n\nIntercept Linear model ", " with ", 
                  levels(community.f), " Event ", zname, "\n\n")
                print(summary(lm(intercept ~ community.f, na.action = na.exclude)))
            }
            maxsall <- maxball <- (-10^10)
            minsall <- minball <- 10^10
            for (j in 1:length(levels(community.f))) {
                transgrey <- add.alpha("grey", translevel)
                cat("\nStarting Community ", levels(community.f)[j], 
                  " ir = ", ir, "\n")
                a <- NULL
                c <- NULL
                r2 <- NULL
                aerror <- NULL
                cerror <- NULL
                z <- NULL
                N <- NULL
                p <- NULL
                maxs <- maxb <- (-10^10)
                mins <- minb <- 10^10
                for (irow in ir) {
                  zero <- Td$ET.x[irow, refindex]
                  zname <- rownames(Td$ET.x)[irow]
                  newb <- reviseb_witherror(rownum = irow, Td = Td, 
                    ref = refindex)
                  slope <- newb$E.s
                  intercept <- newb$E.b
                  x <- 1 - slope[which(community.f == levels(community.f)[j]), 
                    refindex]
                  y <- intercept[which(community.f == levels(community.f)[j]), 
                    refindex]
                  ncomp <- length(which(complete.cases(y, x)))
                  if (ncomp > minpoints) {
                    fit <- lm(y ~ x, na.action = na.exclude)
                    fit_coef <- coef(summary(fit))
                    a <- c(a, fit_coef[2, "Estimate"])
                    aerror <- c(aerror, sse = fit_coef[2, "Std. Error"])
                    c <- c(c, fit_coef["(Intercept)", "Estimate"])
                    cerror <- c(cerror, fit_coef["(Intercept)", 
                      "Std. Error"])
                    r2 <- c(r2, summary(fit)$r.squared)
                    N <- c(N, ncomp)
                    p <- c(p, fit_coef["(Intercept)", "Pr(>|t|)"])
                    z <- c(z, zero)
                  }
                  else {
                    a <- c(a, NA)
                    c <- c(c, NA)
                    r2 <- c(r2, NA)
                    aerror <- c(aerror, NA)
                    cerror <- c(cerror, NA)
                    z <- c(z, NA)
                  }
                  maxsc <- max(x, na.rm = TRUE)
                  maxbc <- max(y, na.rm = TRUE)
                  minsc <- min(x, na.rm = TRUE)
                  minbc <- min(y, na.rm = TRUE)
                  if (minsc < mins) 
                    mins <- minsc
                  if (minbc < minb) 
                    minb <- minbc
                  if (maxsc > maxs) 
                    maxs <- maxsc
                  if (maxbc > maxb) 
                    maxb <- maxbc
                  dat <- cbind(x, y)
                }
                if (mins < minsall) 
                  minsall <- mins
                if (minbc < minball) 
                  minball <- minb
                if (maxs > maxsall) 
                  maxsall <- maxs
                if (maxb > maxball) 
                  maxball <- maxb
                if (is.null(rownum)) {
                  if (length(which(!is.na(a))) != 0) {
                    names(a) <- names(c) <- names(r2) <- names(aerror) <- names(cerror) <- names(z) <- rownames(Td$ET.x)[ir]
                    cat("\n\nGROUPING ", levels(community.f)[j])
                    cat("\nlinear fits for \n", rownames(Td$ET.x)[ir], 
                      "\n is a=\n")
                    print(a)
                    cat("stats on linear fit a (slope)ERROR  a+z=constant=intersection point STD DEVa")
                    valaerror <- nastat(aerror)
                    cat("stats on linear fit c (intercept)ERROR  Group shift STD DEV")
                    valcerror <- nastat(cerror)
                    cat("\nStats on all Intercept c=Group displacements ")
                    valA <- nastat(c)
                    cat("stats on all a+z=constant=intersection point of Average profile ")
                    valY <- nastat(a + z)
                    cat("stats on Number of points ")
                    valN <- nastat(N)
                    cat("stats on all Intercept Probability for no correlation ")
                    valp <- nastat(p)
                    cat("\n Statistical value for Group intersection point(shift from intersection of ", 
                      colnames(Td$ET.x)[refindex], " profile) is ", 
                      valA$m, " std dev(ALL) ", valcerror$m, 
                      " N ", valN$m, " p ", valp$m)
                    cat("\n Statistical value for Reference intersection point for ", 
                      colnames(Td$ET.x)[refindex], " is ", valY$m, 
                      " std dev(ALL) ", valaerror$m)
                    cat("\n            intersection point(from Average value of profile)     is ", 
                      valY$m - Td$l.s.zero, " std dev ", valaerror$m, 
                      "\n")
                    cat("\nAverage profile is \n")
                    print(Td$ET.x[, refindex])
                    cat("\nvalY", valY$m)
                    cat("\nir", ir, "\n")
                    cat(Td$ET.x[, refindex], "\nenter")
                    bestintersectr <- findbest(val = valY$m, 
                      comparelist = Td$ET.x[, refindex], ir = ir)
                    vlow <- valY$m - valaerror$m
                    vupper <- valY$m + valaerror$m
                    cat(vlow, "\nenter")
                    bestintersectlow <- findbest(val = vlow, 
                      comparelist = Td$ET.x[, refindex], ir = ir)
                    cat(vupper, "\nenter")
                    bestintersectup <- findbest(val = vupper, 
                      comparelist = Td$ET.x[, refindex], ir = ir)
                    cat("\n\nSUMMARY: GROUPING ", levels(community.f)[j])
                    if (!is.null(bestintersectr)) {
                      cat("\n best intersection index is", bestintersectr, 
                        " at amounts (low,mid,upper)", vlow, 
                        valY$m, vupper, "\n event (low mid upper) ", 
                        rownames(Td$ET.x)[bestintersectlow], 
                        rownames(Td$ET.x)[bestintersectr], rownames(Td$ET.x)[bestintersectup])
                      bestintersectname <- c(bestintersectname, 
                        rownames(Td$ET.x)[bestintersectlow], 
                        rownames(Td$ET.x)[bestintersectr], rownames(Td$ET.x)[bestintersectup])
                      cat("\nGroup intersection point(shift from intersection of ", 
                        colnames(Td$ET.x)[refindex], " profile) is ", 
                        valA$m, " std dev(ALL) ", valcerror$m)
                    }
                    else {
                      bestintersectname <- c(bestintersectname, 
                        NA, NA, NA)
                    }
                  }
                  else {
                    bestintersectname <- c(bestintersectname, 
                      NA, NA, NA)
                  }
                  mixname <- paste0(levels(community.f)[j])
                  names(bestintersectname)[(length(bestintersectname) - 
                    2):length(bestintersectname)] <- c(paste0("LOW", 
                    mixname), paste0("MID", mixname), paste0("UPPER", 
                    mixname))
                }
                if (!is.null(rownum)) {
                  for (irow in ir) {
                    zero <- Td$ET.x[irow, refindex]
                    cat("\nFirst Start zero ", zero, " with irow ", 
                      irow)
                    zname <- rownames(Td$ET.x)[irow]
                    newb <- reviseb_witherror(rownum = irow, 
                      Td = Td, ref = refindex)
                    slope <- newb$E.s
                    intercept <- newb$E.b
                    x <- 1 - slope[which(community.f == levels(community.f)[j]), 
                      refindex]
                    y <- intercept[which(community.f == levels(community.f)[j]), 
                      refindex]
                    ds <- maxs - mins
                    db <- maxb - minb
                    slim <- c(mins - ds/4, maxs + ds/2)
                    blim <- c(minb - db/4, maxb + db/4)
                    dat <- cbind(x, y)
                    if (irow != ir[1]) 
                      par(new = TRUE)
                    bagplot(dat, na.rm = TRUE, factor = 2.5, 
                      create.plot = TRUE, approx.limit = 300, 
                      show.outlier = TRUE, show.looppoints = TRUE, 
                      show.bagpoints = TRUE, dkmethod = 2, show.whiskers = TRUE, 
                      show.loophull = TRUE, show.baghull = TRUE, 
                      verbose = FALSE, col.looppoints = colourpt, 
                      col.loophull = transgrey, col.baghull = vcolours, 
                      ylab = paste0("Intercept"), xlab = paste("1-slope (-Amplitude relative to Reference", 
                        colnames(slope)[refindex], ")"), cex = 0.8, 
                      xlim = slim, ylim = blim, main = paste("Community ", 
                        levels(community.f)[j], main), cex.main = 0.8)
                    lines(seq(-20, 20, by = 0.01), rep(0, 4001))
                    lines(rep(0, 2001), seq(-1000, 1000, by = 1))
                  }
                  legend("topright", inset = 0, title = "Events", 
                    rownames(Td$ET.x)[ir], fill = vcolours)
                }
            }
            ds <- maxsall - minsall
            db <- maxball - minball
            slim <- c(minsall - ds/8, maxsall + ds/4)
            blim <- c(minball - db/4, maxball + db/8)
            for (j in 1:length(levels(community.f))) {
                cat("\nmin max s", slim, " min max b", blim, 
                  "\n")
                for (irow in ir) {
                  zero <- Td$ET.x[irow, refindex]
                  zname <- rownames(Td$ET.x)[irow]
                  newb <- reviseb_witherror(rownum = irow, Td = Td, 
                    ref = refindex)
                  slope <- newb$E.s
                  intercept <- newb$E.b
                  x <- 1 - slope[which(community.f == levels(community.f)[j]), 
                    refindex]
                  y <- intercept[which(community.f == levels(community.f)[j]), 
                    refindex]
                  dat <- cbind(x, y)
                  bagplot(dat, na.rm = TRUE, factor = 2.5, create.plot = TRUE, 
                    approx.limit = 300, show.outlier = TRUE, 
                    show.looppoints = TRUE, show.bagpoints = TRUE, 
                    dkmethod = 2, show.whiskers = TRUE, show.loophull = TRUE, 
                    show.baghull = TRUE, verbose = FALSE, transparency = TRUE, 
                    ylab = paste0("Intercept"), xlab = paste("1-slope (-Amplitude relative to Reference", 
                      colnames(slope)[refindex], ")"), cex = 0.8, 
                    xlim = slim, ylim = blim, main = paste("Community ", 
                      levels(community.f)[j], "zero=", round(zero, 
                        digits = 2), main, "\nzero event is", 
                      zname), cex.main = 0.8)
                  lines(seq(-20, 20, by = 0.01), rep(0, 4001))
                  lines(rep(0, 2001), seq(-1000, 1000, by = 1))
                }
                if (is.null(rownum)) {
                  irr <- 1
                  for (irow in ir) {
                    zero <- Td$ET.x[irow, refindex]
                    zname <- rownames(Td$ET.x)[irow]
                    newb <- reviseb_witherror(rownum = irow, 
                      Td = Td, ref = refindex)
                    slope <- newb$E.s
                    intercept <- newb$E.b
                    x <- 1 - slope[which(community.f == levels(community.f)[j]), 
                      refindex]
                    y <- intercept[which(community.f == levels(community.f)[j]), 
                      refindex]
                    dat <- cbind(x, y)
                    if (irow != ir[1]) 
                      par(new = TRUE)
                    bagplot(dat, na.rm = TRUE, factor = 2.5, 
                      create.plot = TRUE, approx.limit = 300, 
                      show.outlier = TRUE, show.looppoints = TRUE, 
                      show.bagpoints = TRUE, dkmethod = 2, show.whiskers = TRUE, 
                      show.loophull = TRUE, show.baghull = TRUE, 
                      verbose = FALSE, col.looppoints = colourpt[irr], 
                      col.loophull = transgrey, col.baghull = vcolours[irr], 
                      col.outlier = colourpt[irr], ylab = paste0("Intercept"), 
                      xlab = paste("1-slope (-Amplitude relative to Reference", 
                        colnames(slope)[refindex], ")"), cex = 0.8, 
                      xlim = slim, ylim = blim, main = paste("Community ", 
                        levels(community.f)[j], main), cex.main = 0.8)
                    irr <- irr + 1
                    lines(seq(-20, 20, by = 0.01), rep(0, 4001))
                    lines(rep(0, 2001), seq(-1000, 1000, by = 1))
                  }
                  legend("topright", inset = 0, title = "Events", 
                    rownames(Td$ET.x)[ir], fill = vcolours)
                }
            }
            ds <- maxsall - minsall
            db <- maxball - minball
            slim <- c(minsall - 0 * ds/8, maxsall + ds/4)
            blim <- c(minball - 0 * db/4, maxball + 0 * db/8)
        }
    }
    for (irow in ir) {
        zero <- Td$ET.x[irow, refindex]
        zname <- rownames(Td$ET.x)[irow]
        newb <- reviseb_witherror(rownum = irow, Td = Td, ref = refindex)
        slope <- newb$E.s
        intercept <- newb$E.b
        x <- 1 - slope[, refindex]
        y <- intercept[, refindex]
        dat <- cbind(x, y)
        bagplot(dat, ylab = paste0("Intercept"), xlab = "1-slope (Amplitude)", 
            cex = 0.8, xlim = slim, ylim = blim, main = paste("All data together\nReference", 
                colnames(slope)[refindex], "zero =", round(zero, 
                  digits = 2), "zero event is", zname, main), 
            cex.main = 0.8)
        lines(seq(-20, 20, by = 0.01), rep(0, 4001))
        lines(rep(0, 2001), seq(-1000, 1000, by = 1))
        ncomp <- length(which(complete.cases(y, x)))
        if (ncomp > minpoints) {
            fit <- lm(y ~ x, na.action = na.exclude)
            cat("\n\nLINEAR FIT vs 1-slope for ALL DATA", main, 
                "zero used is ", zero, "\nONLY intercept,error and intercept probability important\n")
            print(summary(fit))
            fit_coef <- coef(summary(fit))
            r2 <- summary(fit)$r.squared
            cat(" a+z=constant=intersection point", fit_coef[2, 
                "Estimate"] + zero, " STD DEVa", fit_coef[2, 
                "Std. Error"])
            Y <- fit_coef[2, "Estimate"] + zero
            cat(" intercept of fit = Group shift", fit_coef["(Intercept)", 
                "Estimate"], " STD DEV", fit_coef["(Intercept)", 
                "Std. Error"])
            cat("\n  N ", ncomp, " p(intercept differs from 0) ", 
                fit_coef["(Intercept)", "Pr(>|t|)"])
            cat("\nir", ir, "\n")
            cat("\nY ", Y, "\nenter ")
            bestintersectr <- findbest(val = Y, comparelist = Td$ET.x[, 
                refindex], ir = (1:nrow(Td$ET.x)))
            vlow <- fit_coef[2, "Estimate"] + zero - fit_coef[2, 
                "Std. Error"]
            vupper <- fit_coef[2, "Estimate"] + zero + fit_coef[2, 
                "Std. Error"]
            cat("\nlower ", vlow)
            bestintersectlow <- findbest(val = vlow, comparelist = Td$ET.x[, 
                refindex], ir = (1:nrow(Td$ET.x)))
            cat("\nupper ", vupper)
            bestintersectup <- findbest(val = vupper, comparelist = Td$ET.x[, 
                refindex], ir = (1:nrow(Td$ET.x)))
            cat("\n\n intersection rownames are LOW MID UPPER ", 
                rownames(Td$ET.x)[bestintersectlow], rownames(Td$ET.x)[bestintersectr], 
                rownames(Td$ET.x)[bestintersectup], "\n\n")
        }
        if (!is.null(community.f)) {
            length(levels(community.f))
            summary(community.f)
            par(mfrow = c(1, 1))
            x <- 1 - slope[which(community.f == levels(community.f)[j]), 
                refindex]
            y <- intercept[which(community.f == levels(community.f)[j]), 
                refindex]
            dat <- cbind(x, y)
            vcolours <- lotscol1[1:length(levels(community.f))]
            colourpt <- lotscol[1:length(levels(community.f))]
            transgrey <- add.alpha("grey", translevel)
            for (j in 1:length(levels(community.f))) {
                x <- 1 - slope[which(community.f == levels(community.f)[j]), 
                  refindex]
                y <- intercept[which(community.f == levels(community.f)[j]), 
                  refindex]
                dat <- cbind(x, y)
                if (j != 1) 
                  par(new = TRUE)
                bagplot(dat, na.rm = TRUE, factor = 2.5, create.plot = TRUE, 
                  approx.limit = 300, show.outlier = TRUE, show.looppoints = TRUE, 
                  show.bagpoints = FALSE, dkmethod = 2, show.whiskers = FALSE, 
                  show.loophull = TRUE, show.baghull = TRUE, 
                  verbose = FALSE, col.looppoints = colourpt[j], 
                  col.loophull = transgrey, col.baghull = vcolours[j], 
                  col.bagpoints = "black", ylab = paste0("Intercept"), 
                  xlab = "1-slope (-Amplitude relative to Ref)", 
                  main = paste("Reference\n", colnames(slope)[refindex], 
                    "zero time=", round(zero, digits = 2), main, 
                    "zero event is", zname), xlim = slim, ylim = blim, 
                  cex.main = 0.9)
                lines(seq(-20, 20, by = 0.01), rep(0, 4001))
                lines(rep(0, 2001), seq(-1000, 1000, by = 1))
            }
            legend("topright", inset = 0, title = "All", levels(community.f), 
                fill = vcolours)
        }
    }
    if (length(ir) != 1) {
        vcolours <- lotscol1[1:length(ir)]
        colourpt <- lotscol[1:length(ir)]
        iir <- 1
        for (irow in ir) {
            zero <- Td$ET.x[irow, refindex]
            zname <- rownames(Td$ET.x)[irow]
            newb <- reviseb_witherror(rownum = irow, Td = Td, 
                ref = refindex)
            slope <- newb$E.s
            intercept <- newb$E.b
            x <- 1 - slope[, refindex]
            y <- intercept[, refindex]
            dat <- cbind(x, y)
            if (irow != ir[1]) 
                par(new = TRUE)
            bagplot(dat, na.rm = TRUE, factor = 2.5, create.plot = TRUE, 
                approx.limit = 300, show.outlier = TRUE, show.looppoints = TRUE, 
                show.bagpoints = TRUE, dkmethod = 2, show.whiskers = TRUE, 
                show.loophull = TRUE, show.baghull = TRUE, verbose = FALSE, 
                col.looppoints = colourpt[iir], col.loophull = transgrey, 
                col.baghull = vcolours[iir], ylab = paste0("Intercept"), 
                xlab = paste("1-slope (-Amplitude relative to Reference", 
                  colnames(slope)[refindex], ")"), cex = 0.8, 
                xlim = slim, ylim = blim, main = paste("All communities together", 
                  main), cex.main = 0.8)
            iir <- iir + 1
            lines(seq(-20, 20, by = 0.01), rep(0, 4001))
            lines(rep(0, 2001), seq(-1000, 1000, by = 1))
        }
        legend("topright", inset = 0, title = "Events", rownames(Td$ET.x)[ir], 
            fill = vcolours)
    }
    return(bestintersectname)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
