\name{findbest}
\alias{findbest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
find best intersections
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
findbest(val, comparelist, ir)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{val}{
%%     ~~Describe \code{val} here~~
}
  \item{comparelist}{
%%     ~~Describe \code{comparelist} here~~
}
  \item{ir}{
%%     ~~Describe \code{ir} here~~
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
function (val, comparelist, ir) 
{
    bestintersectr <- NULL
    if (!is.nan(val)) {
        for (r in 1:(length(ir) - 1)) {
            dist <- abs(val - comparelist[ir[r]])
            disttot <- abs(comparelist[ir[r + 1]] - comparelist[ir[r]])
            if (dist < disttot && val >= comparelist[ir[r]]) {
                if (dist < disttot/2) 
                  bestintersectr <- (r)
                else bestintersectr <- (r + 1)
            }
        }
        if (is.null(bestintersectr)) {
            if (val < comparelist[ir[1]]) 
                bestintersectr <- 1
            else bestintersectr <- length(ir)
        }
    }
    return(bestintersectr)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
