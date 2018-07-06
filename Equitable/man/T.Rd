\name{T}
\alias{T}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transform
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
T(rmult, cmult, FUN = eg4, noise = TRUE, diagonal = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rmult}{
%%     ~~Describe \code{rmult} here~~
}
  \item{cmult}{
%%     ~~Describe \code{cmult} here~~
}
  \item{FUN}{
%%     ~~Describe \code{FUN} here~~
}
  \item{noise}{
%%     ~~Describe \code{noise} here~~
}
  \item{diagonal}{
%%     ~~Describe \code{diagonal} here~~
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
function (rmult, cmult, FUN = eg4, noise = TRUE, diagonal = TRUE) 
{
    d <- FUN(rmult, cmult)
    if (noise) {
        d_noise <- jitter(c(d), factor = 5, amount = 0)
        d_noise <- matrix(d_noise, nrow = nrow(d), ncol = ncol(d))
        rownames(d_noise) <- rownames(d)
        colnames(d_noise) <- colnames(d)
        d <- d_noise
        imagenan(d, main = paste0("Noise: rmult= ", rmult, " cmult= ", 
            cmult, " Jitter=5"))
    }
    Td <- transformE(d, minp = 0.5, mins = 0, minfinal = 0, Ave = TRUE, 
        diagonal = diagonal)
    runstats(Td)
    return(Td)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
