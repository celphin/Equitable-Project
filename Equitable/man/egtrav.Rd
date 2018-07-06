\name{egtrav}
\alias{egtrav}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
travelling wave
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
egtrav(rmult, cmult)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rmult}{
%%     ~~Describe \code{rmult} here~~
}
  \item{cmult}{
%%     ~~Describe \code{cmult} here~~
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
function (rmult, cmult) 
{
    cend <- 10
    rend <- 10
    rnum0 <- 10
    cnum0 <- 10
    r <- seq(rend/rnum0 * (1/rmult), rend, by = rend/rnum0 * 
        (1/rmult))
    c <- seq(cend/cnum0 * (1/cmult), cend, by = cend/cnum0 * 
        (1/cmult))
    f <- rep(1, length(c))
    g <- sin(2 * pi * (r + c)/(rend))
    u <- 0 * c
    ti <- t(1:(rnum0 * rmult))
    x <- t(1:(cnum0 * cmult))
    d <- matrix(NA, nrow = length(ti), ncol = length(x))
    for (a in ti) {
        for (b in x) {
            d[a, b] <- sin(2 * pi * (r[a] + c[b])/(rend))
        }
    }
    rownames(d) <- ti
    colnames(d) <- x
    imagenan(d, main = paste0("SIGNAL: rmult= ", rmult, " cmult= ", 
        cmult))
    Tx <- d
    return(Tx)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
