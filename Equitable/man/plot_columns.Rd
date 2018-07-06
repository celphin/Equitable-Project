\name{plot_columns}
\alias{plot_columns}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot columns
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot_columns(d, x_unit = "INDEX", y_unit = "DATA VALUE", main = "Data plots", limits = NULL, xlimits = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d}{
%%     ~~Describe \code{d} here~~
}
  \item{x_unit}{
%%     ~~Describe \code{x_unit} here~~
}
  \item{y_unit}{
%%     ~~Describe \code{y_unit} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
}
  \item{limits}{
%%     ~~Describe \code{limits} here~~
}
  \item{xlimits}{
%%     ~~Describe \code{xlimits} here~~
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
function (d, x_unit = "INDEX", y_unit = "DATA VALUE", main = "Data plots", 
    limits = NULL, xlimits = NULL) 
{
    if (is.null(limits)) 
        limits <- c(min(d, na.rm = TRUE), max(d, na.rm = TRUE))
    if (is.null(xlimits)) 
        xlimits <- c(1, nrow(d))
    par(mfrow = c(1, 1))
    plot(d[, 1], ylim = limits, xlim = xlimits, main = main, 
        xlab = x_unit, ylab = y_unit, xaxt = "n", yaxt = "n")
    if (is.null(y_unit)) 
        y_unit <- "Data Value"
    if (is.null(x_unit)) 
        x_unit <- "Index"
    rtick <- 1
    er <- nrow(d)
    rtickinc <- (er - rtick)/10
    rnames <- rownames(d)
    axis(1, at = c(seq(rtick, er, by = rtickinc)), labels = rnames[seq(rtick, 
        er, by = rtickinc)], lwd = 2)
    axis(2, ylim = limits, lwd = 2)
    apply(as.data.frame(seq(ncol(d), 1, by = (-1))), 1, FUN = function(v) {
        lines(d[, v], type = "o", pch = v\%\%25, col = v)
    })
    return(limits)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
