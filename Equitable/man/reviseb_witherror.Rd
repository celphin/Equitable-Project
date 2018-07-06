\name{reviseb_witherror}
\alias{reviseb_witherror}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
revise intercept with error
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
reviseb_witherror(rownum = NULL, Td, ref = ncol(Td$ET.x))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rownum}{
%%     ~~Describe \code{rownum} here~~
}
  \item{Td}{
%%     ~~Describe \code{Td} here~~
}
  \item{ref}{
%%     ~~Describe \code{ref} here~~
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
function (rownum = NULL, Td, ref = ncol(Td$ET.x)) 
{
    b_old <- Td$ET.Eb
    b_old1 <- Td$E.b
    s <- Td$ET.Es
    s1 <- Td$E.s
    oldzero <- Td$l.s.zero
    if (is.null(ref)) 
        ref <- ncol(Td$ET.x)
    if (!is.null(rownum)) 
        zero <- Td$ET.x[rownum, ref] - oldzero
    else zero <- 0
    Td$ET.Eb <- b_old - zero * (1 - s)
    Td$E.b <- b_old1 - zero * (1 - s1)
    ssd <- Td$E.sd1
    bsd <- Td$E.bsd1
    zsd <- Td$ET.xsd[rownum, ref]/sqrt(Td$ET.EN[rownum, ref])
    Td$E.bsd1 <- sqrt(((s - 1) * ssd)^2 + bsd^2)
    Td$l.s.zero <- Td$ET.x[rownum, ref]
    return(Td)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
