\name{Txt_from_axr_Irt_bxr}
\alias{Txt_from_axr_Irt_bxr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
tranform from slope intercept and reference data
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Txt_from_axr_Irt_bxr(axr, Irt, bxr = NULL, zero = 0, maxA = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{axr}{
%%     ~~Describe \code{axr} here~~
}
  \item{Irt}{
%%     ~~Describe \code{Irt} here~~
}
  \item{bxr}{
%%     ~~Describe \code{bxr} here~~
}
  \item{zero}{
%%     ~~Describe \code{zero} here~~
}
  \item{maxA}{
%%     ~~Describe \code{maxA} here~~
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
function (axr, Irt, bxr = NULL, zero = 0, maxA = NULL) 
{
    if (!is.null(maxA)) 
        axr[axr > maxA] <- NA
    if (is.null(bxr)) {
        bx <- rep(0, length(axr))
    }
    Ixt <- matrix(NA, nrow = length(Irt), ncol = length(axr))
    colnames(Ixt) <- names(axr)
    rownames(Ixt) <- names(Irt)
    for (t in 1:length(Irt)) {
        Ixt[t, ] <- axr * (Irt[t] - zero) + bxr
    }
    imagenan(Ixt, main = "Data built from a(x,r), b(x,r), I(r,t)")
    Ixt <- Ixt + zero
    return(Ixt)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
