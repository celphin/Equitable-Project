\name{twovarequit}
\alias{twovarequit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
two variable equit
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
twovarequit(a, at)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
%%     ~~Describe \code{a} here~~
}
  \item{at}{
%%     ~~Describe \code{at} here~~
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
function (a, at) 
{
    ma <- max(abs(a), abs(at))
    z <- c(-1, at, -a, 1)
    sr <- polyroot(z)
    cat("\n", sr, "\n")
    realroots <- Re(sr[which(abs(Im(sr)) < 1e-10)])
    cat("\nreal roots", realroots)
    min(realroots - a)
    s <- realroots[which((realroots - a) == min(realroots - a))]
    st <- 1/s[1]
    slope <- list(s = s, st = st)
    cat("\n slope=", slope$s, slope$st)
    z <- c(-1, a, -at, 1)
    sr <- polyroot(z)
    realroots <- Re(sr[which(abs(Im(sr)) < 1e-10)])
    min(realroots - at)
    s <- realroots[which((realroots - at) == min(realroots - 
        at))]
    st <- 1/s[1]
    cat("\n reverse s,st", s, st, "\n")
    return(slope)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
