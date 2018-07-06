\name{make_B}
\alias{make_B}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
make simple equitable intercept matrix
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
make_B(xref, orig, A)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xref}{
%%     ~~Describe \code{xref} here~~
}
  \item{orig}{
%%     ~~Describe \code{orig} here~~
}
  \item{A}{
%%     ~~Describe \code{A} here~~
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
function (xref, orig, A) 
{
    B <- matrix(NA, nrow = nrow(A), ncol = ncol(A))
    cme <- colMeans(orig, na.rm = TRUE)
    plot(cme, pch = 15, type = "b", main = "Time averaged column variation")
    B[, xref] <- cme - A[, xref] * cme[xref]
    B <- sapply(1:ncol(B), function(c) {
        B[, c] <- B[, xref] - A[, c] * B[c, xref]
    })
    sdB <- sd(B, na.rm = TRUE)
    mB <- mean(B, na.rm = TRUE)
    zlim <- c(mB - 1 * sdB, mB + 1 * sdB)
    return(B)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
