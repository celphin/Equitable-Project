\name{plotdata_with_errors}
\alias{plotdata_with_errors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot data with error bars
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotdata_with_errors(dataset, data_std, rnames = 1:length(dataset), main = "data", ylim = c(0, 1), xlim = NULL, xlab = "ROW", ylab = "DATA VALUE", pch = "O", type = "p", col = "black", lty = 1, lineonly = FALSE, cex.main = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataset}{
%%     ~~Describe \code{dataset} here~~
}
  \item{data_std}{
%%     ~~Describe \code{data_std} here~~
}
  \item{rnames}{
%%     ~~Describe \code{rnames} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
}
  \item{xlim}{
%%     ~~Describe \code{xlim} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
}
  \item{pch}{
%%     ~~Describe \code{pch} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{lty}{
%%     ~~Describe \code{lty} here~~
}
  \item{lineonly}{
%%     ~~Describe \code{lineonly} here~~
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
function (dataset, data_std, rnames = 1:length(dataset), main = "data", 
    ylim = c(0, 1), xlim = NULL, xlab = "ROW", ylab = "DATA VALUE", 
    pch = "O", type = "p", col = "black", lty = 1, lineonly = FALSE, 
    cex.main = 1) 
{
    numrows <- length(dataset)
    if (is.null(xlim)) 
        xlim <- c(1, numrows)
    d = data.frame(x = c(1:numrows), y = dataset, xsd = data_std)
    if (is.null(ylab)) 
        ylab <- "Data Value"
    if (is.null(xlab)) 
        xlab <- "Index"
    if (!lineonly) {
        plot(d$x, d$y, pch = pch, ylim = ylim, xlim = xlim, xlab = xlab, 
            ylab = ylab, xaxt = "n", yaxt = "n", cex.lab = 1.5, 
            cex.axis = 1.5, cex.sub = 1.5)
        with(data = d, expr = errbar(x, y, y + xsd, y - xsd, 
            add = TRUE, pch = pch, type = type, col = col, lty = lty, 
            cap = 0.01))
        title(main = main, cex.main = cex.main)
        rtick <- xlim[1]
        er <- xlim[2]
        rtickinc <- round((er - rtick)/10)
        if (rtickinc == 0) 
            rtickinc = 1
        axis(1, at = c(seq(rtick, er, by = rtickinc)), labels = rnames[seq(rtick, 
            er, by = rtickinc)], lwd = 2, cex.lab = 1.5, cex.axis = 1.5, 
            cex.sub = 1.5)
        axis(2, ylim = ylim, lwd = 2, cex.lab = 1.5, cex.axis = 1.5, 
            cex.sub = 1.5)
    }
    else {
        line(d$x, d$y)
        with(data = d, expr = errbar(x, y, y + xsd, y - xsd, 
            add = TRUE, pch = pch, type = type, col = col, lty = lty, 
            cap = 0.01))
    }
    return()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
