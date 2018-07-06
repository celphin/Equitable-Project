\name{make3simpleE}
\alias{make3simpleE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
make 3 but simple
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
make3simpleE(T3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{T3}{
%%     ~~Describe \code{T3} here~~
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
function (T3) 
{
    Td <- T3$Td1
    Tx1 <- make_data_fromE(orig = Td$smat, A = Td$E.s, B = Td$E.b, 
        zero = Td$l.s.zero, main = "Direct Simple 1")
    Td <- T3$Td2
    Tx2 <- make_data_fromE(orig = Td$smat, A = Td$E.s, B = Td$E.b, 
        zero = Td$l.s.zero, main = "Direct Simple 2")
    Td <- T3$Td3
    Tx3 <- make_data_fromE(orig = Td$smat, A = Td$E.s, B = Td$E.b, 
        zero = Td$l.s.zero, main = "Direct Simple 3")
    Tx <- list(e1 = Tx1$Tx, e2 = Tx2$Tx, e3 = Tx3$Tx)
    return(Tx)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
