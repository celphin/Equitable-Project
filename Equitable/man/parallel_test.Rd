\name{parallel_test}
\alias{parallel_test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
test for parallel in least sq fits
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
parallel_test(x, y, slope = 1, scritical = 0.1, pcritical = 1e-05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{slope}{
%%     ~~Describe \code{slope} here~~
}
  \item{scritical}{
%%     ~~Describe \code{scritical} here~~
}
  \item{pcritical}{
%%     ~~Describe \code{pcritical} here~~
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
function (x, y, slope = 1, scritical = 0.1, pcritical = 1e-05) 
{
    fit <- lm(y ~ x, na.action = na.exclude)
    sfit <- summary(fit)
    if (nrow(sfit$coefficients) == 2) {
        tstats <- (slope - sfit$coefficients[2, 1])/sfit$coefficients[2, 
            2]
        pval <- 2 * pt(abs(tstats), df = df.residual(fit), lower.tail = FALSE)
        if (sfit$coefficients[2, "Pr(>|t|)"] < pcritical) {
            if (abs((slope - sfit$coefficients[2, 1])/slope) < 
                scritical) 
                pval = 1
            else pval = 0
        }
    }
    else pval <- NA
    if (is.nan(pval)) 
        pval <- NA
    return(pval)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
