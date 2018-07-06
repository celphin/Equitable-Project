\name{xtdata}
\alias{xtdata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
xt data
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
xtdata(I, x = NULL, t = NULL, imageplot = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{I}{
%%     ~~Describe \code{I} here~~
}
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{t}{
%%     ~~Describe \code{t} here~~
}
  \item{imageplot}{
%%     ~~Describe \code{imageplot} here~~
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
function (I, x = NULL, t = NULL, imageplot = FALSE) 
{
    if (is.null(x)) 
        x <- ncol(I)
    if (is.null(t)) 
        t <- 1:nrow(I)
    if (x[1] == "max") {
        x <- floor(which(abs(I) == max(abs(I), na.rm = TRUE))/nrow(I))
        cat("\nxtdata: x set to max row is ", x)
    }
    if (!is.null(x) && !is.null(t)) {
        d <- matrix(NA, nrow = nrow(I), ncol = ncol(I))
        d[t, x] <- I[t, x]
        rownames(d) <- rownames(I)
        colnames(d) <- colnames(I)
        zlimits <- c(min(d, na.rm = TRUE), max(I, na.rm = TRUE) + 
            1)
        if (imageplot) 
            imagenan(d, main = paste0("Original "), zlim = zlimits/1)
    }
    return(d)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
