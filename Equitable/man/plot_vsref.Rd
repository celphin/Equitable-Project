\name{plot_vsref}
\alias{plot_vsref}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot one versus ref
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot_vsref(d, ref, main = "Data plots", xlab = "Data Value (Reference)", limits = NULL, lty = "p", legf = FALSE, cex.main = 0.8)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d}{
%%     ~~Describe \code{d} here~~
}
  \item{ref}{
%%     ~~Describe \code{ref} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{limits}{
%%     ~~Describe \code{limits} here~~
}
  \item{lty}{
%%     ~~Describe \code{lty} here~~
}
  \item{legf}{
%%     ~~Describe \code{legf} here~~
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
function (d, ref, main = "Data plots", xlab = "Data Value (Reference)", 
    limits = NULL, lty = "p", legf = FALSE, cex.main = 0.8) 
{
    if (length(d[, ref]) < 18 && legf) 
        colourevent <- c((1:length(d[, ref])) + 9)
    else colourevent <- "black"
    ref <- as.numeric(ref)
    if (is.null(limits)) {
        mins <- min(d, na.rm = TRUE)
        maxs <- max(d, na.rm = TRUE)
        ds <- maxs - mins
        xlimits <- ylimits <- c(mins - 1 * ds * 3/8, maxs + ds * 
            3/8)
    }
    else {
        ylimits <- xlimits <- limits
    }
    plot(d[, ref], d[, ref], ylim = ylimits, xlim = xlimits, 
        ylab = "Data Value", xlab = xlab, lwd = 3, cex = 1.5, 
        col = colourevent)
    title(main = main, cex.main = cex.main)
    apply(as.data.frame(seq(1, ncol(d), by = 1)), 1, FUN = function(v, 
        d, ref) {
        lines(d[, ref], d[, v], type = lty, pch = v\%\%25, col = v)
    }, ref, d = as.data.frame(d))
    if (legf) 
        legend("bottomright", inset = 0, (rownames(d)), fill = colourevent)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
