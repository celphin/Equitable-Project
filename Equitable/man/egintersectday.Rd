\name{egintersectday}
\alias{egintersectday}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
example intersect day
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
egintersectday(rmult, cmult, days_between_sample = 0, amp = 40, nfac = 0, g0 = 0, A = 200)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rmult}{
%%     ~~Describe \code{rmult} here~~
}
  \item{cmult}{
%%     ~~Describe \code{cmult} here~~
}
  \item{days_between_sample}{
%%     ~~Describe \code{days_between_sample} here~~
}
  \item{amp}{
%%     ~~Describe \code{amp} here~~
}
  \item{nfac}{
%%     ~~Describe \code{nfac} here~~
}
  \item{g0}{
%%     ~~Describe \code{g0} here~~
}
  \item{A}{
%%     ~~Describe \code{A} here~~
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
function (rmult, cmult, days_between_sample = 0, amp = 40, nfac = 0, 
    g0 = 0, A = 200) 
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
    g <- (r - azero)^3/(azero)^3 * amp + g0
    u <- rep(A, length(c))
    cat("\n intersection set to ", azero, " with value of ", 
        f[1] * g[azero] + u[1], " but amp is ", amp, " displaced by g0=", 
        g0, " and A=", A, "\n")
    F <- matrix(rep(f, 2), nrow = length(f), ncol = 2)
    imagenan(t(F), main = "Function f(x)")
    U <- matrix(rep(u, 2), nrow = length(u), ncol = 2)
    G <- matrix(rep(g, 2), nrow = length(g), ncol = 2)
    imagenan(G, main = "Function g(t)")
    Tx <- make_data(f, g, u, cend, rend, rnum0, cnum0, rmult, 
        cmult)
    sdd <- sd(Tx, na.rm = TRUE)
    d_noise <- Tx + rnorm(prod(dim(Tx)), mean = 0, sd = nfac * 
        sdd)
    Tx <- matrix(d_noise, nrow = nrow(Tx), ncol = ncol(Tx))
    if (days_between_sample != 0) {
        Tx <- days_between_sample * ceiling(Tx/days_between_sample)
    }
    rownames(Tx) <- r
    colnames(Tx) <- c
    imagenan(Tx, main = "Data (x,t) resolution (days) for sampling=", 
        days_between_sample)
    return(Tx)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
