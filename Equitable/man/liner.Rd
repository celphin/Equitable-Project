\name{liner}
\alias{liner}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
fit lines and make data 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
liner(x, y, mat, minnum = 4, minsd = 1e-09)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{mat}{
%%     ~~Describe \code{mat} here~~
}
  \item{minnum}{
%%     ~~Describe \code{minnum} here~~
}
  \item{minsd}{
%%     ~~Describe \code{minsd} here~~
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
function (x, y, mat, minnum = 4, minsd = 1e-09) 
{
    fy <- mat[, y]
    fx <- mat[, x]
    ncomp <- length(which(complete.cases(fy, fx)))
    if (ncomp >= minnum && abs(sd(fx, na.rm = TRUE)) > minsd) {
        fit <- lm(fy ~ fx, na.action = na.exclude)
        fit_coef <- coef(summary(fit))
        if (dim(fit_coef) == 2) {
            p_par <- parallel_test(fx, fy)
            param <- list(s = fit_coef[2, "Estimate"], sse = fit_coef[2, 
                "Std. Error"], b = fit_coef["(Intercept)", "Estimate"], 
                bse = fit_coef["(Intercept)", "Std. Error"], 
                r2 = summary(fit)$r.squared, N = summary(fit)$df[2] + 
                  2, pslope = fit_coef[2, "Pr(>|t|)"], node = 0, 
                p_par = p_par)
        }
        else {
            param <- list(s = NA, sse = NA, b = NA, bse = NA, 
                r2 = NA, N = NA, pslope = NA, node = 1, p_par = NA)
        }
    }
    else {
        param <- list(s = NA, sse = NA, b = NA, bse = NA, r2 = NA, 
            N = ncomp, pslope = NA, node = 1, p_par = NA)
    }
    return(param)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
