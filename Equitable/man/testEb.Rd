\name{testEb}
\alias{testEb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
test for inteercept equit
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
testEb(b, s, minval = 1e-06)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{b}{
%%     ~~Describe \code{b} here~~
}
  \item{s}{
%%     ~~Describe \code{s} here~~
}
  \item{minval}{
%%     ~~Describe \code{minval} here~~
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
function (b, s, minval = 1e-06) 
{
    xm <- mean(abs(c(b)), na.rm = TRUE)
    if (abs(xm) > minval) 
        rtest <- (b + s * t(b))/xm
    else rtest <- 0 * b
    xm <- mean(c(rtest), na.rm = TRUE)
    xsd <- sd(c(rtest), na.rm = TRUE)
    t <- list(xm = xm, xsd = xsd)
    return(t)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
