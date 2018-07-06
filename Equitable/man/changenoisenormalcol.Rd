\name{changenoisenormalcol}
\alias{changenoisenormalcol}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
change noise using column only?
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
changenoisenormalcol(rmult, cmult, start, end, inc, actualFunction)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rmult}{
%%     ~~Describe \code{rmult} here~~
}
  \item{cmult}{
%%     ~~Describe \code{cmult} here~~
}
  \item{start}{
%%     ~~Describe \code{start} here~~
}
  \item{end}{
%%     ~~Describe \code{end} here~~
}
  \item{inc}{
%%     ~~Describe \code{inc} here~~
}
  \item{actualFunction}{
%%     ~~Describe \code{actualFunction} here~~
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
function (rmult, cmult, start, end, inc, actualFunction) 
{
    sdx <- sapply(seq(start, end, by = inc), FUN = function(rmult, 
        cmult, fac, eg, diagonal) {
        cat("\n factor for noise is", fac, "\n")
        Td <- Tnormcol(rmult, cmult, fac, noise = FALSE, FUN = eg, 
            diagonal = diagonal)
        Td_noise <- Tnormcol(rmult, cmult, fac, FUN = eg, diagonal = diagonal)
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
        sdall <- list(fac = fac, rmult = rmult, cmult = cmult, 
            nr = nrow(Td$smat), nc = ncol(Td$smat), sdnoise = sdnoise, 
            sdE = sdE, sdls = sdls, sdEave = sdEave, fsqrt = fsqrt, 
            sdsqrt = sdsqrt, sdcalc = sdcalc)
        return(sdall)
    }, rmult = rmult, cmult = cmult, eg = actualFunction, simplify = TRUE, 
        diagonal = TRUE)
    sdx <- matrix(as.numeric(sdx), nrow = nrow(sdx), ncol = ncol(sdx), 
        dimnames = list(rownames(sdx), colnames(sdx)))
    plot(sdx["fac", ], sdx["sdnoise", ], ylim = c(0, max(sdx["sdnoise", 
        ])), main = paste0(" squares=Equitable,ls=Stars,noise=circles\nNorm col: line= lowest std dev \n", 
        "cmult= ", cmult, " rmult=", rmult))
    lines(sdx["fac", ], sdx["sdsqrt", ])
    lines(sdx["fac", ], sdx["sdls", ], type = "p", pch = 11)
    lines(sdx["fac", ], sdx["sdE", ], type = "p", pch = 15)
    lines(sdx["fac", ], sdx["sdcalc", ])
    lines(sdx["fac", ], sdx["sdEave", ], type = "p", pch = "O")
    return(sdx)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
