\name{TnormNA}
\alias{TnormNA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create transform from example (eg?) and factor of noise and missing fraction of data
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
TnormNA(rmult, cmult, FUN = eg4, fac = 0.5, noise = TRUE, NAfrac = NULL, diagonal = TRUE, Ave = TRUE, Zero = FALSE, imageplot = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rmult}{
%%     ~~Describe \code{rmult} here~~
}
  \item{cmult}{
%%     ~~Describe \code{cmult} here~~
}
  \item{FUN}{
%%     ~~Describe \code{FUN} here~~
}
  \item{fac}{
%%     ~~Describe \code{fac} here~~
}
  \item{noise}{
%%     ~~Describe \code{noise} here~~
}
  \item{NAfrac}{
%%     ~~Describe \code{NAfrac} here~~
}
  \item{diagonal}{
%%     ~~Describe \code{diagonal} here~~
}
  \item{Ave}{
%%     ~~Describe \code{Ave} here~~
}
  \item{Zero}{
%%     ~~Describe \code{Zero} here~~
}
  \item{imageplot}{
%%     ~~Describe \code{imageplot} here~~
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
function (rmult, cmult, FUN = eg4, fac = 0.5, noise = TRUE, NAfrac = NULL, 
    diagonal = TRUE, Ave = TRUE, Zero = FALSE, imageplot = FALSE) 
{
    d <- FUN(rmult, cmult)
    imagenan(d)
    if (noise) {
        sdd <- sd(d, na.rm = TRUE)
        dn <- d + rnorm(prod(dim(d)), mean = 0, sd = fac * sdd)
        dn <- matrix(dn, nrow = nrow(d), ncol = ncol(d))
        rownames(dn) <- rownames(d)
        colnames(dn) <- colnames(d)
        if (!is.null(NAfrac)) {
            numspaces <- NAfrac * prod(dim(dn))
            cat("\nadding ", numspaces, " NA values to ", prod(dim(dn)), 
                " total values")
            spaces <- sample(1:prod(dim(dn)), numspaces, replace = FALSE)
            dnNA <- dn
            dnNA[spaces] <- NA
            dNA <- d
            dNA[spaces] <- NA
        }
        else {
            dnNA <- NULL
            dNA <- NULL
        }
        if (imageplot) {
            imagenan(d, main = paste0("Signal: rmult= ", rmult, 
                " cmult= ", cmult, " fac=", fac))
            if (!is.null(NAfrac)) 
                imagenan(dNA, main = paste0("Signal with NAs : rmult= ", 
                  rmult, " cmult= ", cmult, "\nfac=", fac, " fraction of NA=", 
                  NAfrac))
            imagenan(dn, main = paste0("Noise: rmult= ", rmult, 
                " cmult= ", cmult, " fac=", fac))
            if (!is.null(NAfrac)) 
                imagenan(dnNA, main = paste0("Noise with NAs : rmult= ", 
                  rmult, " cmult= ", cmult, "\nfac=", fac, " fraction of NA=", 
                  NAfrac))
        }
    }
    Td <- transformE(d, diagonal = diagonal, Ave = Ave, Zero = Zero)
    cat("\n\nSTATISTICS OF SIGNAL\n")
    runstats(Td)
    if (!is.null(NAfrac)) {
        TdNA <- transformE(dNA, diagonal = diagonal, Ave = Ave, 
            Zero = Zero)
        cat("\n\nSTATISTICS OF SIGNAL WITH NAS\n")
        runstats(TdNA)
    }
    Tdn <- transformE(dn, diagonal = diagonal, Ave = Ave, Zero = Zero)
    cat("\n\nSTATISTICS OF SIGNAL WITH NOISE\n")
    runstats(Tdn)
    cat("\n\nSTATISTICS OF SIGNAL WITH NOISE COMPARED TO SIGNAL\n")
    runstatsNS(Td, Tdn)
    if (!is.null(NAfrac)) {
        TdnNA <- transformE(dnNA, diagonal = diagonal, Ave = Ave, 
            Zero = Zero)
        cat("\n\nSTATISTICS OF SIGNAL WITH NOISE WITH NAS\n")
        runstats(TdnNA)
        cat("\n\nSTATISTICS OF SIGNAL WITH NOISE WITH NAS COMPARED TO SIGNAL WITH NAS\n")
        runstatsNS(TdNA, TdnNA)
        cat("\n\nSTATISTICS OF SIGNAL WITH NOISE WITH NAS COMPARED TO SIGNAL\n")
        runstatsNS(Td, TdnNA)
    }
    else {
        TdNA <- NULL
        TdnNA <- NULL
    }
    trans4 <- list(Td = Td, Tdn = Tdn, TdNA = TdNA, TdnNA = TdnNA, 
        rmult = rmult, cmult = cmult, fac = fac, NAfrac = NAfrac)
    return(trans4)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
