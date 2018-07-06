\name{plotgroup_with_errors}
\alias{plotgroup_with_errors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot a group with errors
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotgroup_with_errors(smatave, smatsd, main = "", xlim = NULL, ylim = NULL, xlab = "ROW", ylab = "DATA VALUE", inames = NULL, leg = TRUE, maxnum = 8, cex = NULL, cex.main = 0.85)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{smatave}{
%%     ~~Describe \code{smatave} here~~
}
  \item{smatsd}{
%%     ~~Describe \code{smatsd} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
}
  \item{xlim}{
%%     ~~Describe \code{xlim} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
}
  \item{inames}{
%%     ~~Describe \code{inames} here~~
}
  \item{leg}{
%%     ~~Describe \code{leg} here~~
}
  \item{maxnum}{
%%     ~~Describe \code{maxnum} here~~
}
  \item{cex}{
%%     ~~Describe \code{cex} here~~
}
  \item{cex.main}{
%%     ~~Describe \code{cex.main} here~~
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
function (smatave, smatsd, main = "", xlim = NULL, ylim = NULL, 
    xlab = "ROW", ylab = "DATA VALUE", inames = NULL, leg = TRUE, 
    maxnum = 8, cex = NULL, cex.main = 0.85) 
{
    if (is.null(cex)) 
        cex = 0.8
    if (is.null(inames)) 
        inames <- colnames(smatave)
    for (firstnum in seq(1, ncol(smatave), by = maxnum)) {
        if ((firstnum + maxnum - 1) <= ncol(smatave)) 
            endval <- (firstnum + maxnum - 1)
        else endval <- ncol(smatave)
        plotdata_with_errors(smatave[, firstnum], smatsd[, firstnum], 
            rnames = rownames(smatave), main = paste(main, (firstnum - 
                1)/maxnum), xlim = NULL, xlab = xlab, ylab = ylab, 
            ylim = ylim, pch = 16, col = 3, cex.main = cex.main)
        for (j in firstnum:endval) {
            jp <- (j - firstnum + 1)\%\%11
            jc <- (j - firstnum + 1)
            jt <- (j - firstnum + 1)\%\%5
            plotdata_with_errors(smatave[, j], smatsd[, j], rnames = rownames(smatave), 
                main = paste(main, (firstnum - 1)/maxnum), xlim = NULL, 
                xlab = xlab, ylab = ylab, ylim = ylim, pch = (jp + 
                  15), type = "b", col = (jc + 2), lty = jt + 
                  1, lineonly = TRUE, cex.main = cex.main)
        }
        if (leg) 
            legend("topleft", inset = c(0, 0), paste(inames[firstnum:(endval)]), 
                pch = (1:(endval - firstnum + 1)\%\%11 + 15), col = (1:(endval - 
                  firstnum + 1) + 2), lty = (1:(endval - firstnum + 
                  1)\%\%5 + 1), cex = cex, pt.cex = 1)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
