\name{eg7}
\alias{eg7}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
example 7
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
eg7(rmult, cmult)
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
    f <- abs(c - cend/2) + 20
    g <- ceiling(r/(rend/5)) + 1
    u <- 0 * c
    F <- matrix(rep(f, 2), nrow = length(f), ncol = 2)
    imagenan(t(F), main = "Function f(x)")
    G <- matrix(rep(g, 2), nrow = length(g), ncol = 2)
    imagenan(G, main = "Function g(t)")
    cat("\n std f ", sd(f), "   std g ", sd(g), "   std u ", 
        sd(u), "    std(f)std(g) ", sd(f) * sd(g))
    cat("\n sqrt mean (f^2) ", sqrt(mean(f^2)), "   sqrtmean (g^2) ", 
        sqrt(mean(g^2)), "  sqrtmean (u^2) ", sqrt(mean(u^2)), 
        "    sqrt(mean(f^2))*std(g) ", sqrt(mean(f^2)) * sd(g))
    Tx <- make_data(f, g, u, cend, rend, rnum0, cnum0, rmult, 
        cmult)
    return(Tx)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
