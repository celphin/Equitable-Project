\name{reduce_s}
\alias{reduce_s}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
reduce slope matrix 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
reduce_s(s, se, p, ls, lse, node, minp = 0.5, mins = 0, b = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{
%%     ~~Describe \code{s} here~~
}
  \item{se}{
%%     ~~Describe \code{se} here~~
}
  \item{p}{
%%     ~~Describe \code{p} here~~
}
  \item{ls}{
%%     ~~Describe \code{ls} here~~
}
  \item{lse}{
%%     ~~Describe \code{lse} here~~
}
  \item{node}{
%%     ~~Describe \code{node} here~~
}
  \item{minp}{
%%     ~~Describe \code{minp} here~~
}
  \item{mins}{
%%     ~~Describe \code{mins} here~~
}
  \item{b}{
%%     ~~Describe \code{b} here~~
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
function (s, se, p, ls, lse, node, minp = 0.5, mins = 0, b = FALSE) 
{
    blankthreshold <- 0.5
    s1 <- s
    s1[(p > minp | (abs(s1) - se) < mins)] <- NA
    ls[(p > minp | (abs(ls) - lse) < mins)] <- NA
    sp <- s1
    s1[which(is.na(t(s1)))] <- NA
    ls[which(is.na(t(ls)))] <- NA
    blankcols <- apply(s1, 1, FUN = function(x) sum(is.na(x)))/ncol(s1)
    if (b) {
        s1[which(blankcols > blankthreshold), ] <- ls[which(blankcols > 
            blankthreshold), ]
        node[which(blankcols > blankthreshold), ] <- 2
        s1[, which(blankcols > blankthreshold)] <- NA
    }
    else {
        s1[which(blankcols > blankthreshold), ] <- NA
        s1[, which(blankcols > blankthreshold)] <- NA
    }
    s1[row(s1) == col(s1)] <- 1
    s1andnode <- list(s1 = s1, node = node)
    return(s1andnode)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
