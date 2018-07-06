\name{egintersect}
\alias{egintersect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
example intersect
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
egintersect(rmult, cmult)
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
    rnum0 <- 15
    cnum0 <- 10
    rend <- rnum0 * rmult
    r <- seq(rend/rnum0 * (1/rmult), rend, by = rend/rnum0 * 
        (1/rmult))
    c <- seq(cend/cnum0 * (1/cmult), cend, by = cend/cnum0 * 
        (1/cmult))
    f <- (c - 180)/360 + 1
    azero <- (rend/2 - rend/20)
    g <- (r - azero)^3/(azero)^3 * 20
    u <- rep(200, length(c))
    cat("\n intersection set to ", azero, " with value of ", 
        f[1] * g[azero] + u[1], "\n")
    F <- matrix(rep(f, 2), nrow = length(f), ncol = 2)
    imagenan(t(F), main = "Function f(x)")
    U <- matrix(rep(u, 2), nrow = length(u), ncol = 2)
    G <- matrix(rep(g, 2), nrow = length(g), ncol = 2)
    imagenan(G, main = "Function g(t)")
    Tx <- make_data(f, g, u, cend, rend, rnum0, cnum0, rmult, 
        cmult)
    return(Tx)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
