\name{make_data}
\alias{make_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
make data from f g u
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
make_data(f, g, u, cend, rend, rnum0, cnum0, rmult, cmult, pf = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f}{
%%     ~~Describe \code{f} here~~
}
  \item{g}{
%%     ~~Describe \code{g} here~~
}
  \item{u}{
%%     ~~Describe \code{u} here~~
}
  \item{cend}{
%%     ~~Describe \code{cend} here~~
}
  \item{rend}{
%%     ~~Describe \code{rend} here~~
}
  \item{rnum0}{
%%     ~~Describe \code{rnum0} here~~
}
  \item{cnum0}{
%%     ~~Describe \code{cnum0} here~~
}
  \item{rmult}{
%%     ~~Describe \code{rmult} here~~
}
  \item{cmult}{
%%     ~~Describe \code{cmult} here~~
}
  \item{pf}{
%%     ~~Describe \code{pf} here~~
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
function (f, g, u, cend, rend, rnum0, cnum0, rmult, cmult, pf = TRUE) 
{
    t <- 1:(rnum0 * rmult)
    x <- 1:(cnum0 * cmult)
    d <- outer(t, x, Vectorize(FUN = function(t, x, f, g, u) {
        f[x] * g[t] + u[x]
    }), f = as.data.frame(f), u = as.data.frame(u), g = as.data.frame(g))
    rownames(d) <- t
    colnames(d) <- x
    if (pf && nrow(d) > 1 && ncol(d) > 1) 
        imagenan(d, main = paste0("SIGNAL: rmult= ", rmult, " cmult= ", 
            cmult))
    return(d)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
