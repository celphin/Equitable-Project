\name{plotx_vs_y_with_errors}
\alias{plotx_vs_y_with_errors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot one versus another with errors
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotx_vs_y_with_errors(x, y, stdy, xlab = "Reference", ylab = "non-Reference", main = "Non-reference Vs Reference", ylim = c(140, 260), rnames = names(y), pch = 15, cex.main = 0.8)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{stdy}{
%%     ~~Describe \code{stdy} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
}
  \item{rnames}{
%%     ~~Describe \code{rnames} here~~
}
  \item{pch}{
%%     ~~Describe \code{pch} here~~
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
function (x, y, stdy, xlab = "Reference", ylab = "non-Reference", 
    main = "Non-reference Vs Reference", ylim = c(140, 260), 
    rnames = names(y), pch = 15, cex.main = 0.8) 
{
    numrows <- length(y)
    d = data.frame(x = x, y = y, sd = stdy)
    plot(d$x, d$y, xlim = ylim, ylim = ylim, xlab = xlab, ylab = ylab)
    with(data = d, expr = errbar(x, y, y + sd, y - sd, add = TRUE, 
        pch = pch, cap = 0.01))
    title(main = main, cex.main = cex.main)
    lines(ylim[1]:ylim[2], rep(0, length(ylim[1]:ylim[2])))
    lines(rep(0, length(ylim[1]:ylim[2])), ylim[1]:ylim[2])
    lines(ylim[1]:ylim[2], ylim[1]:ylim[2], lty = 2)
    return()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
