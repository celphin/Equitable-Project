\name{AI_Bmult_NA}
\alias{AI_Bmult_NA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simple equitable transform AIB}
\description{Simple equitable transform AIB}
\usage{
AI_Bmult_NA(A, I, B = NULL, zero = 0, maxA = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{
%%     ~~Describe \code{A} here~~
}
  \item{I}{
%%     ~~Describe \code{I} here~~
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
function (A, I, B = NULL, zero = 0, maxA = NULL) 
{
    I <- as.matrix(I - zero)
    if (!is.null(maxA)) 
        A[A > maxA] <- NA
    if (is.null(B)) {
        B <- matrix(0, nrow = nrow(A), ncol = ncol(A))
    }
    AyI_By <- AyI_Byxme <- AyI_Byxsd <- AyI_ByN <- NULL
    for (t in 1:nrow(I)) {
        It <- I[t, ]
        xme <- xsd <- N <- NULL
        AIt_B <- sapply(1:ncol(A), function(y) {
            xme <- mean((c(A[y, ] * It + B[y, ])), na.rm = TRUE)
            xsd <- sd(A[y, ] * It + B[y, ], na.rm = TRUE)
            N <- length(which(!is.na(A[y, ] * It)))
            AIt_B <- list(xme = xme, xsd = xsd, N = N)
            return(AIt_B)
        })
        xme <- unlist(AIt_B["xme", ])
        xsd <- unlist(AIt_B["xsd", ])
        N <- unlist(AIt_B["N", ])
        AyI_Byxme <- cbind(AyI_Byxme, xme)
        AyI_Byxsd <- cbind(AyI_Byxsd, xsd)
        AyI_ByN <- cbind(AyI_ByN, N)
    }
    rownames(AyI_Byxme) <- rownames(AyI_Byxsd) <- rownames(AyI_ByN) <- colnames(I)
    colnames(AyI_Byxme) <- colnames(AyI_Byxsd) <- colnames(AyI_ByN) <- rownames(I)
    AI_B <- list(xme = t(AyI_Byxme + zero), xsd = t(AyI_Byxsd), 
        N = t(AyI_ByN))
    return(AI_B)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
