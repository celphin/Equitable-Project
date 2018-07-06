\name{Tnorm}
\alias{Tnorm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create transform from example (eg?) and factor of noise
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Tnorm(rmult, cmult, FUN = eg4, fac = 0.5, noise = TRUE, diagonal = TRUE, Ave = TRUE, Zero = FALSE)
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
  \item{fac}{
%%     ~~Describe \code{fac} here~~
}
  \item{noise}{
%%     ~~Describe \code{noise} here~~
}
  \item{diagonal}{
%%     ~~Describe \code{diagonal} here~~
}
  \item{Ave}{
%%     ~~Describe \code{Ave} here~~
}
  \item{Zero}{
%%     ~~Describe \code{Zero} here~~
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
function (rmult, cmult, FUN = eg4, fac = 0.5, noise = TRUE, diagonal = TRUE, 
    Ave = TRUE, Zero = FALSE) 
{
    d <- FUN(rmult, cmult)
    if (noise) {
        sdd <- sd(d, na.rm = TRUE)
        d_noise <- d + rnorm(prod(dim(d)), mean = 0, sd = (fac * 
            sdd))
        d_noise <- matrix(d_noise, nrow = nrow(d), ncol = ncol(d))
        rownames(d_noise) <- rownames(d)
        colnames(d_noise) <- colnames(d)
        cat(sd(d), sd(d_noise), sd(d - d_noise))
        d <- d_noise
        imagenan(d, main = paste0("Noise: rmult= ", rmult, " cmult= ", 
            cmult, " fac=", fac))
    }
    Td <- transformE(d, diagonal = diagonal, Ave = Ave, Zero = Zero)
    runstats(Td)
    return(Td)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
