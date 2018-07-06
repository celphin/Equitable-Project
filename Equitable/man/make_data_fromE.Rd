\name{make_data_fromE}
\alias{make_data_fromE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
make data from equitable 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
make_data_fromE(orig, A, B = NULL, zero = 0, maxA = 3, main = " ")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{orig}{
%%     ~~Describe \code{orig} here~~
}
  \item{A}{
%%     ~~Describe \code{A} here~~
}
  \item{B}{
%%     ~~Describe \code{B} here~~
}
  \item{zero}{
%%     ~~Describe \code{zero} here~~
}
  \item{maxA}{
%%     ~~Describe \code{maxA} here~~
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
function (orig, A, B = NULL, zero = 0, maxA = 3, main = " ") 
{
    orig <- as.matrix(orig)
    AI <- AI_Bmult_NA(A = A, I = orig, B = B, maxA = maxA)
    Tx <- AI$xme
    Tsd <- AI$xsd
    TN <- AI$N
    sdo <- sd(orig, na.rm = TRUE)
    mo <- mean(orig, na.rm = TRUE)
    zlim <- c(mo - 1.5 * sdo, mo + 1.5 * sdo)
    imagenan(Tx, main = paste(main, "Equitable"), zlim = zlim)
    imagenan(orig, main = paste(main, "Original"), zlim = zlim)
    cat("\nresidual std (Simple Transform-Orig)", main, sd(Tx - 
        orig))
    imagenan(orig - Tx, main = paste(main, "Residuals"), zlim = zlim)
    plot(Tx, orig, main = paste(main, " vs Original"))
    lines(orig, orig)
    Tsimple <- list(Tx = Tx, Tsd = Tsd, TN = TN)
    return(Tsimple)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
