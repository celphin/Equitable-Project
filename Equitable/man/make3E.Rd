\name{make3E}
\alias{make3E}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
make  3 Equit
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
make3E(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
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
function (x) 
{
    Td1 <- transformE(x, Ave = FALSE, diagonal = FALSE)
    plotsummary(Td1)
    x1 <- Td1$smat - Td1$ET.x
    Td2 <- transformE(x1, Ave = FALSE, diagonal = FALSE)
    plotsummary(Td2)
    x2 <- Td2$smat - Td2$ET.x
    Td3 <- transformE(x2, Ave = FALSE, diagonal = FALSE)
    plotsummary(Td3)
    x3 <- Td3$smat - Td3$ET.x
    T3 <- list(Td1 = Td1, Td2 = Td2, Td3 = Td3, resid = x3)
    return(T3)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
