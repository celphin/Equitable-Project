\name{plotversus}
\alias{plotversus}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
cycle through and plot one versus another
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotversus(orig, E, ls, signal = NULL, Eave, ylimits = NULL, genname = NULL, of = FALSE, lf = FALSE, ef = TRUE, avef = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{orig}{
%%     ~~Describe \code{orig} here~~
}
  \item{E}{
%%     ~~Describe \code{E} here~~
}
  \item{ls}{
%%     ~~Describe \code{ls} here~~
}
  \item{signal}{
%%     ~~Describe \code{signal} here~~
}
  \item{Eave}{
%%     ~~Describe \code{Eave} here~~
}
  \item{ylimits}{
%%     ~~Describe \code{ylimits} here~~
}
  \item{genname}{
%%     ~~Describe \code{genname} here~~
}
  \item{of}{
%%     ~~Describe \code{of} here~~
}
  \item{lf}{
%%     ~~Describe \code{lf} here~~
}
  \item{ef}{
%%     ~~Describe \code{ef} here~~
}
  \item{avef}{
%%     ~~Describe \code{avef} here~~
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
function (orig, E, ls, signal = NULL, Eave, ylimits = NULL, genname = NULL, 
    of = FALSE, lf = FALSE, ef = TRUE, avef = FALSE) 
{
    if (is.null(genname)) 
        genname <- "Data"
    if (is.null(ylimits)) 
        ylimits <- c(min(E, na.rm = TRUE), max(E, na.rm = TRUE))
    if (is.null(signal)) {
        if (of) {
            if (lf) {
                plot(ls, orig, main = "Noise: Original vs Least Squares Transform ", 
                  xlab = "Least Squares Transform", ylab = "Original Data", 
                  ylim = ylimits, xlim = ylimits)
                lines(seq(ylimits[1], ylimits[2], by = (ylimits[2] - 
                  ylimits[1])/100), seq(ylimits[1], ylimits[2], 
                  by = (ylimits[2] - ylimits[1])/100))
            }
            if (ef) {
                plot(E, orig, main = "Noise: Original vs Equitable Transform ", 
                  xlab = "Equitable Transform", ylab = "Original Data", 
                  ylim = ylimits, xlim = ylimits)
                lines(seq(ylimits[1], ylimits[2], by = (ylimits[2] - 
                  ylimits[1])/100), seq(ylimits[1], ylimits[2], 
                  by = (ylimits[2] - ylimits[1])/100))
            }
            if (avef) {
                plot(Eave, orig, main = "Noise: Original vs Equitable Transform REFERENCED", 
                  xlab = "Equitable Transform (Referenced)", 
                  ylab = "Original Data", ylim = ylimits, xlim = ylimits)
                lines(seq(ylimits[1], ylimits[2], by = (ylimits[2] - 
                  ylimits[1])/100), seq(ylimits[1], ylimits[2], 
                  by = (ylimits[2] - ylimits[1])/100))
            }
        }
    }
    else {
        if (of) {
            plot(signal, orig, main = "Noise: Original Data vs Signal", 
                xlab = "Signal", ylab = "Original Data", ylim = ylimits, 
                xlim = ylimits)
            lines(seq(ylimits[1], ylimits[2], by = (ylimits[2] - 
                ylimits[1])/100), seq(ylimits[1], ylimits[2], 
                by = (ylimits[2] - ylimits[1])/100))
        }
        if (lf) {
            plot(signal, ls, main = "Noise: Least Squares Transform vs Signal", 
                xlab = "Signal", ylab = "Least Squares Transform", 
                ylim = ylimits, xlim = ylimits)
            lines(seq(ylimits[1], ylimits[2], by = (ylimits[2] - 
                ylimits[1])/100), seq(ylimits[1], ylimits[2], 
                by = (ylimits[2] - ylimits[1])/100))
        }
        if (ef) {
            plot(signal, E, main = "Noise: Equitable Transform vs Signal", 
                xlab = "Signal", ylab = "Equitable Transform", 
                ylim = ylimits, xlim = ylimits)
            lines(seq(ylimits[1], ylimits[2], by = (ylimits[2] - 
                ylimits[1])/100), seq(ylimits[1], ylimits[2], 
                by = (ylimits[2] - ylimits[1])/100))
        }
        if (avef) {
            plot(signal, Eave, main = "Noise: Equitable Transform REFERENCED vs Signal", 
                xlab = "Signal", ylab = "Equitable Transform REFERENCED", 
                ylim = ylimits, xlim = ylimits)
            lines(seq(ylimits[1], ylimits[2], by = (ylimits[2] - 
                ylimits[1])/100), seq(ylimits[1], ylimits[2], 
                by = (ylimits[2] - ylimits[1])/100))
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
