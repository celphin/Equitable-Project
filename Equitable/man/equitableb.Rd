\name{equitableb}
\alias{equitableb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
equitable intercept calc
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
equitableb(mat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{
%%     ~~Describe \code{mat} here~~
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
function (mat) 
{
    nc <- ncol(mat$b)
    s1 <- mat$E.s
    s1[is.na(t(s1))] <- NA
    b1 <- mat$b
    b1[is.na(s1)] <- NA
    rtest <- testEb(b1, s1)
    cat("Equitableb: test values of least squares fits ", rtest$xm, 
        rtest$xsd, "\n")
    Eb <- outer(1:nc, 1:nc, Vectorize(Eintercept), b = as.data.frame(c(b1)), 
        s = as.data.frame(c(s1)), nc = nc, p = as.data.frame(c(mat$pslope)))
    colnames(Eb) <- colnames(mat$b)
    rownames(Eb) <- rownames(mat$b)
    Eb1 <- Eb
    Eb1sd <- outer(1:nc, 1:nc, Vectorize(Esdintercept), b = as.data.frame(c(b1)), 
        s = as.data.frame(c(s1)), nc = nc, p = as.data.frame(c(mat$pslope)))
    colnames(Eb1sd) <- colnames(mat$s)
    rownames(Eb1sd) <- rownames(mat$s)
    Eb1N <- outer(1:nc, 1:nc, Vectorize(bN), b = as.data.frame(c(b1)), 
        s = as.data.frame(c(s1)), nc = nc, p = as.data.frame(c(mat$pslope)))
    colnames(Eb1N) <- colnames(mat$s)
    rownames(Eb1N) <- rownames(mat$s)
    numrun = 1
    rtest <- testEb(Eb, s1)
    bElist <- list(b = Eb, numrun = numrun, rtestbm = rtest$xm, 
        rtestbsd = rtest$xsd, b1 = Eb1, bsd1 = Eb1sd, bN = Eb1N)
    bElist <- equitb(bElist, Eb, s1, mat$pslope)
    bElist$b[which(is.na(bElist$b))] <- mat$b[which(is.na(bElist$b))]
    bElist$b1[which(is.na(bElist$b1))] <- mat$b[which(is.na(bElist$b1))]
    bElist$bsd1[which(is.na(bElist$bsd1))] <- 0
    return(bElist)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
