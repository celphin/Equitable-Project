\name{egintersecttwo}
\alias{egintersecttwo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
example intersect two regions
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
egintersecttwo(rmult, cmult, days_between_sample = 0, amp = 40, amp2 = 20, A = 200, A2 = 210, nfac = 0, g0 = 0, g2 = 5)
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
  \item{amp2}{
%%     ~~Describe \code{amp2} here~~
}
  \item{A}{
%%     ~~Describe \code{A} here~~
}
  \item{A2}{
%%     ~~Describe \code{A2} here~~
}
  \item{nfac}{
%%     ~~Describe \code{nfac} here~~
}
  \item{g0}{
%%     ~~Describe \code{g0} here~~
}
  \item{g2}{
%%     ~~Describe \code{g2} here~~
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
function (rmult, cmult, days_between_sample = 0, amp = 40, amp2 = 20, 
    A = 200, A2 = 210, nfac = 0, g0 = 0, g2 = 5) 
{
    d <- egintersectday(10, 5, amp = amp, g0 = g0, A = A)
    plot(d[, 20])
    d1 <- egintersectday(10, 5, amp = amp2, g0 = g2, A = A2)
    plot(d1[, 20])
    lines(d[, 20])
    colnames(d) <- paste0(colnames(d1), "_a", amp, "_g", g0, 
        "_A", A)
    colnames(d1) <- paste0(colnames(d1), "_a", amp2, "_g", g2, 
        "_A", A2)
    Tx <- cbind(d, d1)
    imagenan(Tx)
    return(Tx)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
