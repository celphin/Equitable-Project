\name{corre}
\alias{corre}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
correlate all pairs
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
corre(matr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{matr}{
%%     ~~Describe \code{matr} here~~
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
function (matr) 
{
    nc <- ncol(matr)
    val <- sapply(1:nc, FUN = runy, mat = matr)
    val1 <- array(as.numeric(val), dim = c(nc, 9, nc))
    colnames(val) <- colnames(matr)
    m <- list(s = t(val1[, 1, ]), sse = t(val1[, 2, ]), b = t(val1[, 
        3, ]), bse = t(val1[, 4, ]), r2 = t(val1[, 5, ]), N = t(val1[, 
        6, ]), pslope = t(val1[, 7, ]), node = t(val1[, 8, ]), 
        p_par = t(val1[, 9, ]))
    m$pslope[which(is.nan(m$pslope))] <- NA
    if (length(colnames(matr)) != 0) 
        rown <- paste0("y_", colnames(matr))
    else rown <- colnames(matr)
    coln <- colnames(matr)
    rownames(m$s) <- rownames(m$sse) <- rownames(m$b) <- rownames(m$bse) <- rownames(m$r2) <- rownames(m$N) <- rownames(m$pslope) <- rownames(m$node) <- rownames(m$p_par) <- rown
    colnames(m$s) <- colnames(m$sse) <- colnames(m$b) <- colnames(m$bse) <- colnames(m$r2) <- colnames(m$N) <- colnames(m$pslope) <- colnames(m$node) <- colnames(m$p_par) <- coln
    return(m)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
