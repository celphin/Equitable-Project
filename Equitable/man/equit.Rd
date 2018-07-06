\name{equit}
\alias{equit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
recursive to equitable
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
equit(E, p, r2max = 0.001, semax = 0.01, maxrun = 8)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{E}{
%%     ~~Describe \code{E} here~~
}
  \item{p}{
%%     ~~Describe \code{p} here~~
}
  \item{r2max}{
%%     ~~Describe \code{r2max} here~~
}
  \item{semax}{
%%     ~~Describe \code{semax} here~~
}
  \item{maxrun}{
%%     ~~Describe \code{maxrun} here~~
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
function (E, p, r2max = 0.001, semax = 0.01, maxrun = 8) 
{
    nc <- ncol(E$s)
    cat("Slope: iteration ", E$numrun, " Average R^2 test ", 
        E$rtestxm, " Average std dev of R^2 ", E$rtestxsd, " \n")
    if (is.na(E$rtestxsd)) {
        E$rtestxsd <- 0
        cat("\nRtest std reset to 0")
    }
    if ((abs(E$rtestxm - 1) < r2max && E$rtestxsd < semax) || 
        E$numrun >= maxrun) {
        return(E)
    }
    else {
        E$numrun <- E$numrun + 1
        Es <- outer(1:nc, 1:nc, Vectorize(Eslope), s = as.data.frame(c(E$s)), 
            nc = nc, p = as.data.frame(c(p)))
        colnames(Es) <- colnames(E$s)
        rownames(Es) <- rownames(E$s)
        E$s <- Es
        rtest <- testE(Es)
        E$rtestxm <- rtest$xm
        E$rtestxsd <- rtest$xsd
        E$s <- Es
        E <- equit(E, p)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
