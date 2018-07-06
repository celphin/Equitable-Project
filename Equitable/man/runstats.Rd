\name{runstats}
\alias{runstats}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
run stasts on trnaform info
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
runstats(Tx)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Tx}{
%%     ~~Describe \code{Tx} here~~
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
function (Tx) 
{
    sumNA <- sum(length(which(is.na(Tx$smat))))
    Nt <- prod(dim(Tx$smat))
    fr <- sumNA/Nt
    cat("\ntotal of NA transformed data is ", sumNA, " with total ", 
        Nt, " Fraction of data set that is NA is ", fr)
    fac <- sqrt(1 - fr)
    newM <- nrow(Tx$smat) * fac
    newN <- ncol(Tx$smat) * fac
    sdfactornew <- sqrt(1/(newN - 1) + 1/(newM - 1))
    sdfactor <- sqrt(1/(nrow(Tx$smat) - 1) + 1/(ncol(Tx$smat) - 
        1))
    cat("\n  col= ", ncol(Tx$smat), "  row= ", nrow(Tx$smat), 
        " initial scale factor= ", sdfactor)
    cat("\neffective  col= ", newN, "effective  row= ", newM, 
        " scale factor= ", sdfactornew)
    cat("\noriginal data\n")
    nastat(Tx$smat)
    minmax(Tx$smat)
    cat("Least squared transform \n")
    nastat(Tx$l.s.x)
    minmax(Tx$l.s.x)
    cat("Equitable transform \n")
    nastat(Tx$ET.x)
    minmax(Tx$ET.x)
    cat("Least squared transform -Original data\n")
    nastat(Tx$l.s.x - Tx$smat)
    cat("Equitable transform -Original data\n")
    residE <- nastat(Tx$ET.x - Tx$smat)
    cat("Equitable transform using ref column (Ave usually) -Original data\n")
    nastat(Tx$Ave.ET.x - Tx$smat)
    cat("\n")
    return(residE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
