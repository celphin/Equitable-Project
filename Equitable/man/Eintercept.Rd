\name{Eintercept}
\alias{Eintercept}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find Equitable intercept values
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Eintercept(y, x, b, s, nc, p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{b}{
%%     ~~Describe \code{b} here~~
}
  \item{s}{
%%     ~~Describe \code{s} here~~
}
  \item{nc}{
%%     ~~Describe \code{nc} here~~
}
  \item{p}{
%%     ~~Describe \code{p} here~~
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
function (y, x, b, s, nc, p) 
{
    b <- matrix(b, nrow = nc, ncol = nc)
    s <- matrix(s, nrow = nc, ncol = nc)
    p <- matrix(p, nrow = nc, ncol = nc)
    bx_ = b[x, ]
    by_ <- b[y, ]
    syx <- s[y, x]
    py_ <- p[y, ]
    px_ <- p[x, ]
    pyx <- p[y, x]
    j <- which(complete.cases(bx_, by_))
    if (length(j) > 0 && !is.na(syx)) {
        xvect <- by_[j] - syx * bx_[j]
        wvect <- (1 - py_[j]) * (1 - px_[j])^2 * (1 - pyx)^2
        xm <- weighted.mean(xvect, wvect, na.rm = TRUE)
    }
    else {
        xm <- b[y, x]
    }
    return(xm)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
