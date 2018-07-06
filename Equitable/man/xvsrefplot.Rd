\name{xvsrefplot}
\alias{xvsrefplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot vs ref
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
xvsrefplot(Td, cgroup = NULL, ylim = NULL, ref = NULL, br = "", pf = TRUE, extranames = NULL, err95 = TRUE, fitl = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Td}{
%%     ~~Describe \code{Td} here~~
}
  \item{cgroup}{
%%     ~~Describe \code{cgroup} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
}
  \item{ref}{
%%     ~~Describe \code{ref} here~~
}
  \item{br}{
%%     ~~Describe \code{br} here~~
}
  \item{pf}{
%%     ~~Describe \code{pf} here~~
}
  \item{extranames}{
%%     ~~Describe \code{extranames} here~~
}
  \item{err95}{
%%     ~~Describe \code{err95} here~~
}
  \item{fitl}{
%%     ~~Describe \code{fitl} here~~
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
function (Td, cgroup = NULL, ylim = NULL, ref = NULL, br = "", 
    pf = TRUE, extranames = NULL, err95 = TRUE, fitl = TRUE) 
{
    if (!is.null(ref)) {
        nref <- which(colnames(Td$smat) == "Row_Ave")[1]
        if (ref == "Row_Ave") {
            if (length(nref) != 0) 
                ref <- nref
            else ref <- NULL
        }
    }
    if (is.null(ylim)) {
        mins <- min(Td$smat[, cgroup], na.rm = TRUE)
        maxs <- max(Td$smat[, cgroup], na.rm = TRUE)
        ds <- maxs - mins
        if (nrow(Td$ET.x) <= 21) 
            ylim <- c(mins - 1 * ds * 3/8, maxs + ds * 3/8)
        else ylim <- c(mins - 1 * ds * 3/8, maxs + ds * 0/8)
        if ((ylim[2] - ylim[1]) < 1e-10) 
            ylim <- c(0, 1)
    }
    yrange <- NULL
    if (is.null(cgroup)) 
        cgr <- 1:ncol(Td$ET.x)
    else cgr <- cgroup
    for (c in cgr) {
        yrange <- c(yrange, abs(min(Td$ET.x[, c], na.rm = TRUE) - 
            max(Td$ET.x[, c], na.rm = TRUE)))
    }
    ref1 <- cgr[which(max(yrange, na.rm = TRUE) == yrange)]
    ref1 <- ref1[1]
    if (is.null(ref1)) {
        cat("\nref not set :initializing to 1")
        ref <- ref1 <- 1
    }
    if (!is.null(ref)) {
        refrange <- abs(min(Td$ET.x[, ref], na.rm = TRUE) - max(Td$ET.x[, 
            ref], na.rm = TRUE))
        if (refrange/abs(ylim[2] - ylim[1]) < 0.1 || is.null(ref)) 
            ref <- ref1
    }
    else ref <- ref1
    rmean <- Td$ET.x[, ref]
    rmeano <- Td$smat[, ref]
    for (c in cgroup) {
        y <- Td$ET.x[, c]
        yo <- Td$smat[, c]
        stdy <- Td$ET.xsd[, c]
        ps <- Td$l.s.pslope[c, ref]
        r2 <- Td$l.s.r2[c, ref]
        s <- Td$E.s[c, ref]
        b <- Td$E.b[c, ref]
        if (err95) {
            se <- 2 * Td$E.sd1[c, ref]/sqrt(Td$E.sN[c, ref])
            be <- 2 * Td$E.bsd1[c, ref]/sqrt(Td$E.bN[c, ref])
            sN <- Td$E.sN[c, ref]
            bN <- Td$E.bN[c, ref]
        }
        else {
            se <- NULL
            be <- NULL
            sN <- NULL
            bN <- NULL
        }
        if (!is.numeric(ref)) 
            gref <- which(colnames(Td$ET.x) == ref)
        else gref <- ref
        if (pf) {
            if (fitl) {
                if (is.null(extranames)) 
                  plotp1vsp2(rmean = rmean, rmeano = rmeano, 
                    y = y, yo = yo, stdy = stdy, ylim = ylim, 
                    ps = ps, r2 = r2, s = s, b = b, se = se, 
                    be = be, sN = sN, bN = bN, br = paste0(br), 
                    refname = colnames(Td$ET.x)[gref], cname = colnames(Td$ET.x)[c])
                else {
                  plotp1vsp2(rmean = rmean, rmeano = rmeano, 
                    y = y, yo = yo, stdy = stdy, ylim = ylim, 
                    ps = ps, r2 = r2, s = s, b = b, se = se, 
                    be = be, sN = sN, bN = bN, br = paste0(br), 
                    refname = extranames[gref], cname = extranames[c])
                }
            }
            else {
                if (is.null(extranames)) 
                  plot1vs2(rmeano = rmeano, yo = yo, ylim = ylim, 
                    br = paste0(br), refname = colnames(Td$ET.x)[gref], 
                    cname = colnames(Td$ET.x)[c])
                else {
                  plot1vs2(rmeano = rmeano, yo = yo, ylim = ylim, 
                    br = paste0(br), refname = extranames[gref], 
                    cname = extranames[c])
                }
            }
        }
    }
    return(ref1)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
