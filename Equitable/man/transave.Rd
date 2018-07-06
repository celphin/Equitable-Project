\name{transave}
\alias{transave}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
transform from average profile
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
transave(smat, Em, equita = TRUE, x = NULL, t = NULL, diagonal = TRUE, zero = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{smat}{
%%     ~~Describe \code{smat} here~~
}
  \item{Em}{
%%     ~~Describe \code{Em} here~~
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
  \item{zero}{
%%     ~~Describe \code{zero} here~~
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
function (smat, Em, equita = TRUE, x = NULL, t = NULL, diagonal = TRUE, 
    zero = 0) 
{
    d_ave <- xtdata(smat, x = x, t = t)
    d_ave <- d_ave - zero
    cat("\ntransave: zero used is ", zero)
    ETave <- transf(d_ave, Em, equita = equita, diagonal = diagonal)
    names(ETave) <- paste0("Ave.ET.", names(ETave))
    ETave$Ave.ET.x <- ETave$Ave.ET.x + zero
    return(ETave)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
