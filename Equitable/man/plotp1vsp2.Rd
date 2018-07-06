\name{plotp1vsp2}
\alias{plotp1vsp2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot one versus another
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotp1vsp2(rmean, rmeano, y, yo, stdy, ylim = NULL, ps = 1, r2 = 0, s = NULL, b = NULL, se = NULL, be = NULL, sN = NULL, bN = NULL, refname = "unknown", cname = "unknown", br = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rmean}{
%%     ~~Describe \code{rmean} here~~
}
  \item{rmeano}{
%%     ~~Describe \code{rmeano} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{yo}{
%%     ~~Describe \code{yo} here~~
}
  \item{stdy}{
%%     ~~Describe \code{stdy} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
}
  \item{ps}{
%%     ~~Describe \code{ps} here~~
}
  \item{r2}{
%%     ~~Describe \code{r2} here~~
}
  \item{s}{
%%     ~~Describe \code{s} here~~
}
  \item{b}{
%%     ~~Describe \code{b} here~~
}
  \item{se}{
%%     ~~Describe \code{se} here~~
}
  \item{be}{
%%     ~~Describe \code{be} here~~
}
  \item{sN}{
%%     ~~Describe \code{sN} here~~
}
  \item{bN}{
%%     ~~Describe \code{bN} here~~
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
function (rmean, rmeano, y, yo, stdy, ylim = NULL, ps = 1, r2 = 0, 
    s = NULL, b = NULL, se = NULL, be = NULL, sN = NULL, bN = NULL, 
    refname = "unknown", cname = "unknown", br = "") 
{
    lotscol <- colors()[c(24, 94, 26, 124, 633, 450, 453, 11, 
        68, 254, 257, 51, 630, 76, 142, 150, 653)]
    lotscol <- c(lotscol, "darkorchid", "darkkhaki", "lightpink", 
        "lightskyblue")
    if (length(unique(names(y))) <= length(lotscol) && length(unique(names(y))) > 
        1) {
        colourevent <- lotscol[1:length(unique(names(y)))]
        if (length(unique(names(yo))) <= 8) 
            colourevent <- c((1:length(y)) + 9)
        if (length(unique(names(y))) != length(y)) {
            colourevent <- rep(NA, length(y))
            for (j in 1:length(unique(names(y)))) {
                colourevent[which(names(y) == unique(names(y))[j])] <- j
            }
        }
    }
    else colourevent <- "black"
    plotx_vs_y_with_errors(x = rmean, y = y, stdy = stdy, ylim = ylim, 
        xlab = paste0("Ref. ", refname), ylab = paste0("Non-Ref. ", 
            cname), main = paste0(br, "\nNon-ref. ", cname, "\nVs Ref. ", 
            refname), cex.main = 0.7)
    points(rmeano, yo, pch = 19, cex = 1.5, col = colourevent)
    lines(rmean, y, lty = 1)
    if (length(unique(names(y))) < 25 && length(unique(names(y))) > 
        1) {
        if (length(unique(names(y))) != length(y)) {
            legend("bottomright", inset = 0, (unique(names(y))), 
                fill = colourevent)
        }
        else legend("bottomright", inset = 0, (names(y)), fill = colourevent)
    }
    legend <- c("Equitable", "Slope of 1", "Original")
    lwd <- c(1, 1, NA)
    lty <- c(1, 2, NA)
    pch <- c(15, NA, 19)
    legend("topleft", inset = 0.02, legend = legend, lwd = lwd, 
        lty = lty, pch = pch, bg = "white", ncol = c(2), cex = 0.75)
    legend("bottomleft", legend = c(paste0("p= ", round(ps, digits = 5)), 
        paste0("Coef.Det.= ", round(r2, digits = 3))))
    if (!is.null(s) && !is.null(b)) {
        if (!is.null(se) && !is.null(be)) 
            legend("bottom", legend = c(paste0("Slope= ", round(s, 
                digits = 2)), paste0("(95\% ", round(se, digits = 2), 
                ") N=", sN), paste0("Inter.= ", round(b, digits = 1)), 
                paste0("(95\% ", round(be, digits = 1), ") N=", 
                  bN)))
        else legend("bottom", legend = c(paste0("Slope= ", round(s, 
            digits = 2)), paste0("Intercept.= ", round(b, digits = 1))))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
