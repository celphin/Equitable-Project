\name{plotdensity}
\alias{plotdensity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot density
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotdensity(s, genname = "Slope", num = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{
%%     ~~Describe \code{s} here~~
}
  \item{genname}{
%%     ~~Describe \code{genname} here~~
}
  \item{num}{
%%     ~~Describe \code{num} here~~
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
function (s, genname = "Slope", num = NULL) 
{
    if (is.null(num)) 
        num <- ncol(I) - 1
    if (length(num) == 1) 
        listx <- seq(1, ncol(I), by = (ncol(I) - 1)/num)
    else listx <- num
    cat(listx)
    sapply(listx, function(c) {
        breaks <- 100
        mu <- summary(abs(s[c, ]), na.rm = TRUE)
        msd <- sd(abs(s[c, ]), na.rm = TRUE)
        mh <- hist(abs(s[c, ]), prob = 1, breaks = breaks, xlim = c(0, 
            10), main = paste(genname, ": row=", c), xlab = "Slope value", 
            ylim = c(0, 1.5))
        den <- density(as.vector(abs(s[c, ])), na.rm = TRUE)
        lines(den, col = 2, lty = 1, lwd = 2)
        lines(rep(mu[2], 200), seq(0.01, 2, by = 0.01), lty = 4, 
            lwd = 2)
        lines(rep(mu[3], 200), seq(0.01, 2, by = 0.01), lty = 4, 
            lwd = 4)
        lines(rep(mu[5], 200), seq(0.01, 2, by = 0.01), lty = 4, 
            lwd = 2)
        lines(rep(mu[4], 200), seq(0.01, 2, by = 0.01), lty = 1, 
            lwd = 4)
    })
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
