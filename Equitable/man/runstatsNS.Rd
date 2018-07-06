\name{runstatsNS}
\alias{runstatsNS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
run stats with NS
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
runstatsNS(Tx, T_noise)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Tx}{
%%     ~~Describe \code{Tx} here~~
}
  \item{T_noise}{
%%     ~~Describe \code{T_noise} here~~
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
function (Tx, T_noise) 
{
    cat("\nSIGNAL \noriginal signal\n")
    storig <- nastat(Tx$smat)
    sigorig <- as.numeric(storig[2])
    cat("Least squared transform \n")
    nastat(T_noise$l.s.x)
    cat("Equitable transform \n")
    stT <- nastat(T_noise$ET.x)
    sigT <- as.numeric(stT[2])
    cat("(Signal+noise) -Original signal  =Noise only\n")
    statN <- nastat(T_noise$smat - Tx$smat)
    sig0 <- as.numeric(statN[2])
    cat("Least squared transform -Original signal\n")
    statls <- nastat(T_noise$l.s.x - Tx$smat)
    sigls <- as.numeric(statls[2])
    cat("Equitable transform -Original signal\n")
    nastat(T_noise$ET.x - Tx$smat)
    cat("Equitable transform using ref column (Ave usually)-Original signal\n")
    nastat(T_noise$Ave.ET.x - Tx$smat)
    a2S <- mean(Tx$l.s.s^2, na.rm = TRUE)
    a2 <- mean(Tx$E.s^2, na.rm = TRUE)
    factora2 <- sqrt(1/nrow(Tx$smat) + a2/ncol(Tx$smat))
    factor <- sqrt(1/(nrow(Tx$smat) - 1) + 1/(ncol(Tx$smat) - 
        1))
    factor_row <- sqrt(1/nrow(Tx$smat))
    factor_col <- sqrt(1/ncol(Tx$smat))
    factora2S <- sqrt(1/nrow(Tx$smat) + a2S/ncol(Tx$smat))
    acolm <- colMeans(Tx$ET.Es, na.rm = TRUE)
    am <- mean(acolm^2, na.rm = TRUE)
    amfactor <- sqrt(1/nrow(Tx$smat) + am/ncol(Tx$smat))
    acolmabs <- colMeans(1 - abs(Tx$E.s), na.rm = TRUE)
    amean <- mean(abs(acolmabs), na.rm = TRUE)
    sumNA <- sum(length(which(is.na(T_noise$smat))))
    Nt <- prod(dim(T_noise$smat))
    fr <- sumNA/Nt
    cat("\ntotal of NA transformed data is ", sumNA, " with total ", 
        Nt, " Fraction of data set that is NA is ", fr)
    fac <- sqrt(1 - fr)
    newM <- nrow(T_noise$smat) * fac
    newN <- ncol(T_noise$smat) * fac
    sdfactornew <- sqrt(1/(newN - 1) + 1/(newM - 1))
    sdfactor <- sqrt(1/(nrow(T_noise$smat) - 1) + 1/(ncol(T_noise$smat) - 
        1))
    cat("\n  col= ", ncol(T_noise$smat), "  row= ", nrow(T_noise$smat), 
        " initial scale factor= ", sdfactor)
    cat("\neffective  col= ", newN, "effective  row= ", newM, 
        " scale factor= ", sdfactornew, "final = ", sig0 * sdfactornew)
    cat("\nfactor of noise reduction no slope : ", factor, " final reduced noise could be ", 
        sig0 * factor)
    cat("\nav am ", am, " factor  : ", amfactor, " final reduced noise should be ", 
        sig0 * amfactor)
    cat("\n mean of 1-abs(slope)", amean)
    lm.ls_vs_sig <- lm(c(T_noise$l.s.x) ~ c(Tx$smat), na.action = na.exclude)
    b <- coef(lm.ls_vs_sig)[1]
    a <- coef(lm.ls_vs_sig)[2]
    cat("\nLeast squared vs signal linear fit \nslope= ", a, 
        " intercept = ", b, "\n")
    sdcalc <- sqrt((a - 1)^2 * sigorig^2 + (a^2) * (sig0 * factor)^2 + 
        b^2)
    ms <- mean(Tx$smat, na.rm = TRUE)
    sdcalc <- sqrt((a - 1)^2 * sigorig^2 + (a^2) * (sig0 * factor)^2 + 
        b^2 + (a - 1) * b * ms)
    sdcalcE <- sqrt((1 - 1)^2 * sigorig^2 + (1^2) * (sig0 * factor)^2 + 
        0^2)
    cat("\n fit l.s vs signal: 1-slope=", 1 - a, " intercept=", 
        b, " signal std dev ", sigorig, "noise sig reduced= ", 
        sig0 * factor, "l.s. Final calc=", sdcalc)
    cat("\n")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
