\name{make_datafrom_eigen}
\alias{make_datafrom_eigen}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
make data from eigenvector
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
make_datafrom_eigen(eigenvect, orig, xref = 1, sgn = 1, main = " ")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{eigenvect}{
%%     ~~Describe \code{eigenvect} here~~
}
  \item{orig}{
%%     ~~Describe \code{orig} here~~
}
  \item{xref}{
%%     ~~Describe \code{xref} here~~
}
  \item{sgn}{
%%     ~~Describe \code{sgn} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
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
function (eigenvect, orig, xref = 1, sgn = 1, main = " ") 
{
    orig <- as.matrix(orig)
    f <- eigenvect
    cm <- colMeans(orig, na.rm = TRUE)
    mcm <- t(matrix(rep(cm, nrow(orig)), nrow = ncol(orig), ncol = nrow(orig)))
    newd <- orig - mcm
    sdc <- sapply(1:ncol(newd), function(c) {
        sd(newd[, c], na.rm = TRUE)
    })
    F <- matrix(rep(f, length(f)), nrow = length(f), ncol = length(f))
    F <- sapply(1:ncol(F), function(c) {
        F[, c] <- F[, c]/F[c, c]
    })
    testd <- F \%*\% t(newd)/ncol(F)
    sdc <- sapply(1:ncol(newd), function(c) {
        sd(newd[, c] - t(testd)[, c], na.rm = TRUE)/sd(newd[, 
            c], na.rm = TRUE)
    })
    F <- matrix(rep(f, length(f)), nrow = length(f), ncol = length(f))
    F <- sapply(1:ncol(F), function(c) {
        F[, c] <- F[, c]/F[c, c]
    })
    testd <- -F \%*\% t(newd)/ncol(F)
    sdc1 <- sapply(1:ncol(newd), function(c) {
        sd(newd[, c] - t(testd)[, c], na.rm = TRUE)/sd(newd[, 
            c], na.rm = TRUE)
    })
    if (min(sdc) < min(sdc1)) {
        sgn <- 1
        xref <- which(sdc == min(sdc))
    }
    else {
        sgn <- (-1)
        xref <- which(sdc1 == min(sdc1))
    }
    cat("\nsign ", sgn, " ref ", xref, "\n")
    cat("\nSIGN ", sgn)
    A <- make_A(xref, f)
    B <- make_B(xref = xref, orig = sgn * orig, A = A)
    Tdata <- make_data_fromE(orig = (orig), A = (sgn * A), B = B, 
        zero = 0, main = main, maxA = 50)
    Tx <- Tdata$Tx
    return(Tdata)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
