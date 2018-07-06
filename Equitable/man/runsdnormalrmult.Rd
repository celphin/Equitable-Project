\name{runsdnormalrmult}
\alias{runsdnormalrmult}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
run sd col with mult
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
runsdnormalrmult(start, end, rmult, fac, actualFunction, Ave = TRUE, diagonal = TRUE, pf = FALSE, ipf = FALSE, C = 1.028)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{start}{
%%     ~~Describe \code{start} here~~
}
  \item{end}{
%%     ~~Describe \code{end} here~~
}
  \item{rmult}{
%%     ~~Describe \code{rmult} here~~
}
  \item{fac}{
%%     ~~Describe \code{fac} here~~
}
  \item{actualFunction}{
%%     ~~Describe \code{actualFunction} here~~
}
  \item{Ave}{
%%     ~~Describe \code{Ave} here~~
}
  \item{diagonal}{
%%     ~~Describe \code{diagonal} here~~
}
  \item{pf}{
%%     ~~Describe \code{pf} here~~
}
  \item{ipf}{
%%     ~~Describe \code{ipf} here~~
}
  \item{C}{
%%     ~~Describe \code{C} here~~
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
function (start, end, rmult, fac, actualFunction, Ave = TRUE, 
    diagonal = TRUE, pf = FALSE, ipf = FALSE, C = 1.028) 
{
    sdx <- sapply(start:end, FUN = function(rmult, cmult, fac, 
        eg, Ave, diagonal, ipf, pf, C) {
        Td <- Tnorm(rmult, cmult, fac, noise = FALSE, FUN = eg, 
            Ave = Ave, diagonal = diagonal)
        Td_noise <- Tnorm(rmult, cmult, fac, FUN = eg, Ave = Ave, 
            diagonal = diagonal)
        runstatsNS(Td, Td_noise)
        sdE <- sd(Td_noise$ET.x - Td$ET.x, na.rm = TRUE)
        sdls <- sd(Td_noise$l.s.x - Td$ET.x, na.rm = TRUE)
        sdEave <- sd(Td_noise$Ave.ET.x - Td$ET.x, na.rm = TRUE)
        sdnoise <- sd(Td_noise$smat - Td$ET.x, na.rm = TRUE)
        fsqrt <- sqrt(1/nrow(Td$smat) + 1/ncol(Td$smat))
        sdsqrt <- fsqrt * sdnoise
        sigorig <- sd(Td_noise$smat, na.rm = TRUE)
        lm.ls_vs_sig <- lm(c(Td_noise$l.s.x) ~ c(Td$smat), na.action = na.exclude)
        b <- coef(lm.ls_vs_sig)[1]
        a <- coef(lm.ls_vs_sig)[2]
        cat("\nLeast squared vs signal linear fit \n")
        ms <- mean(Td$smat, na.rm = TRUE)
        sdcalc <- sqrt((a - 1)^2 * sigorig^2 + (a^2) * (sdsqrt)^2 + 
            b^2 + (a - 1) * b * ms)
        stdvalues <- stats_residuals(Td_noise, Td = Td, ipf = ipf, 
            pf = pf, C = C)
        sdall <- list(rmult = rmult, cmult = cmult, nr = nrow(Td$smat), 
            nc = ncol(Td$smat), sdnoise = sdnoise, theory_sdN_Ave_plus = stdvalues$theory_sdN_Ave_plus, 
            sdnoise_approx = stdvalues$sdnoise_approx, theory_sdN_Ave = stdvalues$theory_sdN_Ave, 
            sdE = sdE, sdls = sdls, sdEave = sdEave, fsqrt = fsqrt, 
            sdsqrt = sdsqrt, sdcalc = sdcalc)
        return(sdall)
    }, rmult = rmult, fac = fac, eg = actualFunction, simplify = TRUE, 
        Ave = Ave, diagonal = diagonal, ipf = ipf, pf = pf, C = C)
    sdx <- matrix(as.numeric(sdx), nrow = nrow(sdx), ncol = ncol(sdx), 
        dimnames = list(rownames(sdx), colnames(sdx)))
    plot(sdx["nc", ], sdx["sdnoise", ], ylim = c(0, max(sdx["sdnoise", 
        ])), ylab = "Standard Deviation", xlab = "Number of Columns", 
        main = paste0("Standard deviation vs Number of Columns \n", 
            " rmult= ", rmult, " std fac=", fac, " C=", C))
    pch <- t((c(1, 15, 11, NA, NA, NA, NA)))
    legend <- c("Noise:I-S", "T-S", "L.S.-S", "Row/Col Ave Noise", 
        "Predicted:L.S.(Bias)", "Predicted Noise", "Predicted:T-S")
    lty <- c(NA, NA, NA, 1, 3, 4, 5)
    lwd <- c(NA, NA, NA, 4, 2, 3, 2)
    legend("bottomleft", inset = 0, legend = legend, lwd = lwd, 
        lty = lty, pch = pch, bg = "white", ncol = c(2), cex = 0.75)
    lines(sdx["nc", ], sdx["theory_sdN_Ave", ], lty = 1, lwd = 4)
    lines(sdx["nc", ], sdx["sdls", ], type = "p", pch = 11)
    lines(sdx["nc", ], sdx["sdE", ], type = "p", pch = 15)
    lines(sdx["nc", ], sdx["sdcalc", ], lty = 3, lwd = 2)
    lines(sdx["nc", ], sdx["sdnoise_approx", ], lty = 4, lwd = 4)
    lines(sdx["nc", ], sdx["theory_sdN_Ave_plus", ], lty = 5, 
        lwd = 2)
    return(sdx)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
