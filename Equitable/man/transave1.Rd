\name{transave1}
\alias{transave1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
trans ave 1
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
transave1(smat, TE, equita = TRUE, x = NULL, t = NULL, diagonal = TRUE, Zero = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{smat}{
%%     ~~Describe \code{smat} here~~
}
  \item{TE}{
%%     ~~Describe \code{TE} here~~
}
  \item{equita}{
%%     ~~Describe \code{equita} here~~
}
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{t}{
%%     ~~Describe \code{t} here~~
}
  \item{diagonal}{
%%     ~~Describe \code{diagonal} here~~
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
function (smat, TE, equita = TRUE, x = NULL, t = NULL, diagonal = TRUE, 
    Zero = FALSE) 
{
    Em <- list(TE$l.s.s, TE$l.s.sse, TE$l.s.b, TE$l.s.bse, TE$l.s.r2, 
        TE$l.s.N, TE$l.s.pslope, TE$l.s.node, TE$E.s, TE$E.b)
    names(Em) <- c("s", "sse", "b", "bse", "r2", "N", "pslope", 
        "node", "p_par", "E.s", "E.b")
    if (Zero) 
        zero <- TE$l.s.zero
    else zero = 0
    cat("\ntransave1: zero used is ", zero)
    ETave <- transave(smat, Em, equita = equita, x = x, t = t, 
        diagonal = diagonal, zero = zero)
    return(ETave)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
