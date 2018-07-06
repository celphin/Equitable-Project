\name{eqfit}
\alias{eqfit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
fit to equitable
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
eqfit(N, M, fac = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{N}{
%%     ~~Describe \code{N} here~~
}
  \item{M}{
%%     ~~Describe \code{M} here~~
}
  \item{fac}{
%%     ~~Describe \code{fac} here~~
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
function (N, M, fac = 1) 
{
    strue <- runif(N, min = -10, max = 10)
    strue
    x <- seq(1, M)
    y <- strue[1] * x
    sdd <- sd(x, na.rm = TRUE)
    xn <- x + rnorm(M, mean = 0, sd = fac * sdd)
    yn <- y + rnorm(M, mean = 0, sd = fac * sdd)
    fit <- lm(yn ~ xn, na.action = na.exclude)
    fit_coef <- coef(summary(fit))
    a = fit_coef[2, "Estimate"]
    sse = fit_coef[2, "Std. Error"]
    b = fit_coef["(Intercept)", "Estimate"]
    bse = fit_coef["(Intercept)", "Std. Error"]
    r2 = summary(fit)$r.squared
    N = summary(fit)$df[2] + 2
    pslope = fit_coef[2, "Pr(>|t|)"]
    at <- r2/a
    cat("\ntrue slopes=", strue, 1/strue, " fitted slopes=", 
        a, at, "r2= ", r2)
    slopes <- twovarequit(a, at)
    lss <- c(1, a, at, 1)
    lss <- matrix(lss, nrow = 2, ncol = 2)
    bt <- (-1) * at * b
    lsb <- c(0, b, bt, 0)
    lsb <- matrix(lsb, nrow = 2, ncol = 2)
    E.s <- c(1, slopes$s, slopes$st, 1)
    E.s <- matrix(E.s, nrow = 2, ncol = 2)
    ps <- c(0, pslope, pslope, 0)
    ps <- matrix(ps, nrow = 2, ncol = 2)
    mat <- list(s = lss, b = lsb, E.s = E.s, pslope = ps)
    Eb <- equitableb(mat)
    bnew <- Eb$b[2, 1]
    cat("\n oldb =", b, " new b=", bnew)
    yeq <- slopes$s * xn + bnew
    yls <- a * xn + b
    plot(xn, yn, main = " squares=eq,3=triangles=ls")
    lines(xn, yeq, type = "p", pch = 15)
    lines(xn, yls, type = "p", pch = 2)
    lines(x, y, type = "o", pch = 1)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
