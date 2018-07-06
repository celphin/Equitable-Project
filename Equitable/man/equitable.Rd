\name{equitable}
\alias{equitable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
equitbale calc
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
equitable(mat)
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
    cat("\n Average 1-p : ", mean(1 - mat$pslope, na.rm = TRUE), 
        "\n")
    nc <- ncol(mat$s)
    s1 <- mat$s
    rtest <- testE(s1)
    cat("Equitable: Average R^2 test ", rtest$xm, " Average std dev of R^2 ", 
        rtest$xsd, " \n")
    s1andnode <- reduce_s(s1, mat$sse, mat$pslope, mat$s, mat$sse, 
        mat$node)
    s1 <- s1andnode$s1
    node <- s1andnode$node
    mat$b <- s1andnode$b1
    rtest <- testE(s1)
    cat("Equitable: Before iteration (Reduced slopes) Average R^2 test ", 
        rtest$xm, " Average std dev of R^2 ", rtest$xsd, " \n\n")
    Es <- outer(1:nc, 1:nc, Vectorize(Eslope), s = as.data.frame(c(s1)), 
        nc = nc, p = as.data.frame(c(mat$pslope)))
    colnames(Es) <- colnames(mat$s)
    rownames(Es) <- rownames(mat$s)
    E1s <- Es
    E1sd <- outer(1:nc, 1:nc, Vectorize(Esd), s = as.data.frame(c(s1)), 
        nc = nc, p = as.data.frame(c(mat$pslope)))
    colnames(E1sd) <- colnames(mat$s)
    rownames(E1sd) <- rownames(mat$s)
    E1sN <- outer(1:nc, 1:nc, Vectorize(sN), s = as.data.frame(c(s1)), 
        nc = nc, p = as.data.frame(c(mat$pslope)))
    colnames(E1sN) <- colnames(mat$s)
    rownames(E1sN) <- rownames(mat$s)
    rtest <- testE(Es)
    numrun = 1
    E <- list(s = Es, numrun = numrun, rtestxm = rtest$xm, rtestxsd = rtest$xsd, 
        s1 = E1s, sd1 = E1sd, sN = E1sN, snode = node)
    E <- equit(E, mat$pslope)
    E$s[which(is.na(E$s))] <- mat$s[which(is.na(E$s))]
    E$s1[which(is.na(E$s1))] <- mat$s[which(is.na(E$s1))]
    E$sd1[which(is.na(E$s1))] <- mat$sse[which(is.na(E$s1))]
    er <- matrix(0, nrow = nrow(E$s), ncol = ncol(E$s))
    s1andnode <- reduce_s(E$s, mat$sse, mat$pslope, mat$s, mat$sse, 
        mat$node, b = TRUE)
    E$s <- s1andnode$s1
    E$snode <- s1andnode$node
    return(E)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
