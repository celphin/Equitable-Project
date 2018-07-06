\name{eg4rand}
\alias{eg4rand}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
eg4 and random function
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
eg4rand(rmult, cmult)
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
    cend <- 360
    rend <- 360
    rnum0 <- 15
    cnum0 <- 10
    r <- seq(rend/rnum0 * (1/rmult), rend, by = rend/rnum0 * 
        (1/rmult))
    c <- seq(cend/cnum0 * (1/cmult), cend, by = cend/cnum0 * 
        (1/cmult))
    f <- ((c/cend)^(1/2) + 3) * sin(2 * pi * (c + 20)/720)
    g <- sin(2 * pi * (r + 30)/270) + sin(2 * pi * (r + 30)/180)
    u <- 3 * (c/cend + 1)
    Tx <- make_data(f, g, u, cend, rend, rnum0, cnum0, rmult, 
        cmult)
    newcol <- sample(1:ncol(Tx), ncol(Tx), replace = F)
    newrow <- sample(1:nrow(Tx), nrow(Tx), replace = F)
    Tx[, newcol] <- Tx[, 1:ncol(Tx)]
    Tx[newrow, ] <- Tx[1:nrow(Tx), ]
    return(Tx)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
