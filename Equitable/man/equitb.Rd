\name{equitb}
\alias{equitb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
equit intercept
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
equitb(E, Eb, s1, p, maxzero_std = 0.1, zero = 0.001, maxrun = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{E}{
%%     ~~Describe \code{E} here~~
}
  \item{Eb}{
%%     ~~Describe \code{Eb} here~~
}
  \item{s1}{
%%     ~~Describe \code{s1} here~~
}
  \item{p}{
%%     ~~Describe \code{p} here~~
}
  \item{maxzero_std}{
%%     ~~Describe \code{maxzero_std} here~~
}
  \item{zero}{
%%     ~~Describe \code{zero} here~~
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
function (E, Eb, s1, p, maxzero_std = 0.1, zero = 0.001, maxrun = 3) 
{
    nc <- ncol(E$b)
    cat("Intercept: ", E$numrun, E$rtestbm, E$rtestbsd, "\n")
    if ((abs(E$rtestbm) < zero && E$rtestbsd < maxzero_std) || 
        E$numrun >= maxrun) {
        return(E)
    }
    else {
        E$numrun <- E$numrun + 1
        Eb <- outer(1:nc, 1:nc, Vectorize(Eintercept), b = as.data.frame(c(Eb)), 
            s = as.data.frame(c(s1)), nc = nc, p = as.data.frame(c(p)))
        colnames(Eb) <- colnames(s1)
        rownames(Eb) <- rownames(s1)
        rtest <- testEb(Eb, s1)
        E$rtestbm <- rtest$xm
        E$rtestbsd <- rtest$xsd
        E$b <- Eb
        E <- equitb(E, Eb, s1, p)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
