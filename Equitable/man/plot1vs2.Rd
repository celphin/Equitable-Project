\name{plot1vs2}
\alias{plot1vs2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot one versus another
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot1vs2(rmeano, yo, ylim = NULL, refname = "unknown", cname = "unknown", br = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rmeano}{
%%     ~~Describe \code{rmeano} here~~
}
  \item{yo}{
%%     ~~Describe \code{yo} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
}
  \item{refname}{
%%     ~~Describe \code{refname} here~~
}
  \item{cname}{
%%     ~~Describe \code{cname} here~~
}
  \item{br}{
%%     ~~Describe \code{br} here~~
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
function (rmeano, yo, ylim = NULL, refname = "unknown", cname = "unknown", 
    br = "") 
{
    if (is.null(ylim)) {
        mins <- min(c(yo, rmeano), na.rm = TRUE)
        maxs <- max(c(yo, rmeano), na.rm = TRUE)
        ds <- maxs - mins
        if (length(yo) <= 21) 
            ylim <- c(mins - 1 * ds * 3/8, maxs + ds * 3/8)
        else ylim <- c(mins - 1 * ds * 3/8, maxs + ds * 0/8)
        if ((ylim[2] - ylim[1]) < 1e-10) 
            ylim <- c(0, 1)
    }
    lotscol <- colors()[c(24, 94, 26, 124, 633, 450, 453, 11, 
        68, 254, 257, 51, 630, 76, 142, 150, 653)]
    lotscol <- c(lotscol, "darkorchid", "darkkhaki", "lightpink", 
        "lightskyblue")
    if (length(unique(names(yo))) <= length(lotscol) && length(unique(names(yo))) > 
        1) {
        colourevent <- lotscol[1:length(unique(names(yo)))]
        if (length(unique(names(yo))) <= 8) 
            colourevent <- c((1:length(yo)) + 9)
        if (length(unique(names(yo))) != length(yo)) {
            colourevent <- rep(NA, length(yo))
            for (j in 1:length(unique(names(yo)))) {
                colourevent[which(names(yo) == unique(names(yo))[j])] <- j
            }
        }
    }
    else colourevent <- "black"
    plot(rmeano, yo, pch = 19, cex = 2, col = colourevent, ylim = ylim, 
        xlim = ylim, xlab = paste0("Ref. ", refname), ylab = paste0("Non-Ref. ", 
            cname), main = paste0(br, "\nNon-ref. ", cname, "\nVs Ref. ", 
            refname), cex.main = 1)
    lines(rmeano, rmeano, lty = 1)
    if (length(unique(names(yo))) < 25 && length(unique(names(yo))) > 
        1) {
        if (length(unique(names(yo))) != length(yo)) {
            legend("bottomright", inset = 0, (unique(names(yo))), 
                fill = colourevent)
        }
        else legend("bottomright", inset = 0, (names(yo)), fill = colourevent)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
