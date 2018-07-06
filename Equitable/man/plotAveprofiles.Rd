\name{plotAveprofiles}
\alias{plotAveprofiles}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot ave profiles
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotAveprofiles(Td96 = Td, main = "", xlim = NULL, ylim = NULL, xlab = "ROW", ylab = "DATA VALUE")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Td96}{
%%     ~~Describe \code{Td96} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
}
  \item{xlim}{
%%     ~~Describe \code{xlim} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
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
function (Td96 = Td, main = "", xlim = NULL, ylim = NULL, xlab = "ROW", 
    ylab = "DATA VALUE") 
{
    if (length(which(colnames(Td96$smat) == "Row_Ave")) != 0) {
        smat <- Td96$smat[, which(colnames(Td96$smat) != "Row_Ave")]
        Ex <- Td96$ET.x[, which(colnames(Td96$ET.x) != "Row_Ave")]
        Exsd <- Td96$ET.xsd[, which(colnames(Td96$ET.xsd) != 
            "Row_Ave")]
    }
    else {
        smat <- Td96$smat
        Ex <- Td96$ET.x
        Exsd <- Td96$ET.xsd
    }
    if (is.null(ylim)) {
        mi <- min(c(Td96$smat, Td96$ET.x), na.rm = TRUE)
        ma <- max(c(Td96$smat, Td96$ET.x), na.rm = TRUE)
        m0 <- mi + 0.2 * (ma - mi)
        mf <- ma - 0.2 * (ma - mi)
        ylim <- c(m0, mf)
    }
    aveprofileO <- rowMeans(smat, na.rm = TRUE)
    sdprofileO <- NULL
    NprofileO <- NULL
    for (r in 1:nrow(smat)) {
        sdprofileO <- c(sdprofileO, sd(smat[r, ], na.rm = TRUE))
        NprofileO <- c(NprofileO, length(which(!is.na(smat[r, 
            ]))))
    }
    names(sdprofileO) <- names(NprofileO) <- names(aveprofileO)
    aveprofile <- rowMeans(Ex, na.rm = TRUE)
    sdprofile <- NULL
    Nprofile <- NULL
    for (r in 1:nrow(Ex)) {
        sdprofile <- c(sdprofile, sd(Ex[r, ], na.rm = TRUE))
        Nprofile <- c(Nprofile, length(which(!is.na(Ex[r, ]))))
    }
    names(sdprofile) <- names(Nprofile) <- names(aveprofile)
    plotdata_with_errors(aveprofileO, sdprofileO, rnames = rownames(smat), 
        main = paste("Average Original (Std. Dev. from Direct Average) of\n", 
            main), ylim = ylim, xlim = NULL, xlab = xlab, ylab = ylab, 
        pch = (15), cex.main = 0.85)
    plotdata_with_errors(aveprofile, sdprofile, rnames = rownames(Ex), 
        main = paste("Average Equitable (Std. Dev. from Direct Average)\nCalc. from", 
            main), ylim = ylim, xlim = NULL, xlab = xlab, ylab = ylab, 
        pch = (15), cex.main = 0.85)
    if (length(which(colnames(Td96$smat) == "Row_Ave")) != 0) {
        plotdata_with_errors(Td96$ET.x[, "Row_Ave"], Td96$ET.xsd[, 
            "Row_Ave"], rnames = rownames(Ex), main = paste("Average Equitable (Std. Dev. using Equitable Errors) \n", 
            main), ylim = ylim, xlim = NULL, xlab = xlab, ylab = ylab, 
            pch = (15), cex.main = 0.85)
    }
    plotdata_with_errors(aveprofileO, sdprofileO/sqrt(NprofileO), 
        rnames = rownames(smat), main = paste("Average Original (Std. Error of Mean from Direct Average) of\n", 
            main), ylim = ylim, xlim = NULL, xlab = xlab, ylab = ylab, 
        pch = (15), cex.main = 0.85)
    plotdata_with_errors(aveprofile, sdprofile/sqrt(Nprofile), 
        rnames = rownames(Ex), main = paste("Average Equitable (Std. Error of Mean from Direct Average) of\n", 
            main), ylim = ylim, xlim = NULL, xlab = xlab, ylab = ylab, 
        pch = (15), cex.main = 0.85)
    if (length(which(colnames(Td96$smat) == "Row_Ave")) != 0) {
        plotdata_with_errors(Td96$ET.x[, "Row_Ave"], Td96$ET.xsd[, 
            "Row_Ave"]/sqrt(Td96$ET.EN[, "Row_Ave"]), rnames = rownames(Td96$ET.x), 
            main = paste("Average Equitable (Std. Error of Mean using Equitable Errors) \n", 
                main), ylim = ylim, xlim = NULL, xlab = xlab, 
            ylab = ylab, pch = (15), cex.main = 0.85)
    }
    names(aveprofile) <- rownames(Td96$ET.x)
    if (length(which(colnames(Td96$smat) == "Row_Ave")) != 0) {
        profileinfo <- list(aveprofileO = aveprofileO, sdprofileO = sdprofileO, 
            NprofileO = NprofileO, aveprofile = aveprofile, sdprofile = sdprofile, 
            Nprofile = Nprofile, aveprofileE = Td96$ET.x[, "Row_Ave"], 
            sdprofileE = Td96$ET.xsd[, "Row_Ave"], NprofileE = Td96$ET.EN[, 
                "Row_Ave"])
    }
    else {
        profileinfo <- list(aveprofileO = aveprofileO, sdprofileO = sdprofileO, 
            NprofileO = NprofileO, aveprofile = aveprofile, sdprofile = sdprofile, 
            Nprofile = Nprofile)
    }
    return(profileinfo)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
