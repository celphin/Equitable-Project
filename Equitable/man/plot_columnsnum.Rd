\name{plot_columnsnum}
\alias{plot_columnsnum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot select columns
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot_columnsnum(d, main = "Data plots", xlab = "Index", limits = NULL, xlimits = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d}{
%%     ~~Describe \code{d} here~~
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
function (d, main = "Data plots", xlab = "Index", limits = NULL, 
    xlimits = NULL) 
{
    if (is.null(limits)) 
        limits <- c(min(d, na.rm = TRUE), max(d, na.rm = TRUE))
    if (is.null(xlimits)) 
        xlimits <- c(1, ncol(d))
    plot(d[1, ], ylim = limits, xlim = xlimits, main = main, 
        ylab = "Data Value", xlab = xlab)
    apply(as.data.frame(seq(2, nrow(d), by = 1)), 1, FUN = function(v) {
        lines(d[v, ], type = "o", pch = paste0(v\%\%25), col = v)
    })
    return(limits)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
