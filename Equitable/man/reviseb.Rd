\name{reviseb}
\alias{reviseb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
revise intercept matrix 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
reviseb(zero, b_old, s)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{zero}{
%%     ~~Describe \code{zero} here~~
}
  \item{b_old}{
%%     ~~Describe \code{b_old} here~~
}
  \item{s}{
%%     ~~Describe \code{s} here~~
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
function (zero, b_old, s) 
{
    cat("\n", ncol(b_old))
    cat("\n", ncol(s))
    cat("\n", zero)
    b <- b_old - zero * (1 - s)
    return(b)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
