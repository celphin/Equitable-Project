\name{plotsummary}
\alias{plotsummary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summary plots of relevant information about the tranform
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotsummary(Td_noise, Td = NULL, Td_old = NULL, row_unit = NULL, col_unit = NULL, z_unit = NULL, yline = 3, yma = 5, fintersect = FALSE, fsquares = FALSE, fpca = FALSE, fave = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Td_noise}{
%%     ~~Describe \code{Td_noise} here~~
}
  \item{Td}{
%%     ~~Describe \code{Td} here~~
}
  \item{Td_old}{
%%     ~~Describe \code{Td_old} here~~
}
  \item{row_unit}{
%%     ~~Describe \code{row_unit} here~~
}
  \item{col_unit}{
%%     ~~Describe \code{col_unit} here~~
}
  \item{z_unit}{
%%     ~~Describe \code{z_unit} here~~
}
  \item{yline}{
%%     ~~Describe \code{yline} here~~
}
  \item{yma}{
%%     ~~Describe \code{yma} here~~
}
  \item{fintersect}{
%%     ~~Describe \code{fintersect} here~~
}
  \item{fsquares}{
%%     ~~Describe \code{fsquares} here~~
}
  \item{fpca}{
%%     ~~Describe \code{fpca} here~~
}
  \item{fave}{
%%     ~~Describe \code{fave} here~~
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
function (Td_noise, Td = NULL, Td_old = NULL, row_unit = NULL, 
    col_unit = NULL, z_unit = NULL, yline = 3, yma = 5, fintersect = FALSE, 
    fsquares = FALSE, fpca = FALSE, fave = FALSE) 
{
    if (!is.null(Td)) 
        runstatsNS(Td, Td_noise)
    plotsquares(Td_noise, signal = Td, of = TRUE, row_unit = row_unit, 
        col_unit = col_unit, z_unit = z_unit)
    if (fsquares) {
        plotsquares(Td_noise, signal = Td, transpose = TRUE, 
            images = FALSE, columns = TRUE, of = TRUE, row_unit = row_unit, 
            col_unit = col_unit, z_unit = z_unit)
        plotsquares(Td_noise, signal = Td, images = FALSE, indiv = TRUE, 
            of = TRUE, errb = TRUE, row_unit = row_unit, col_unit = col_unit, 
            z_unit = z_unit)
        plotsquares(Td_noise, signal = Td, images = FALSE, indiv = TRUE, 
            of = TRUE, errb = TRUE, stderror = TRUE, lf = TRUE, 
            row_unit = row_unit, col_unit = col_unit, z_unit = z_unit)
    }
    plotsome(T = Td_noise, signal = Td$smat, transpose = TRUE, 
        images = FALSE, indiv = TRUE, of = TRUE, lf = TRUE, errb = TRUE, 
        row_unit = row_unit, col_unit = col_unit, z_unit = z_unit)
    plotsome(T = Td_noise, signal = Td$smat, images = FALSE, 
        indiv = TRUE, of = TRUE, lf = TRUE, errb = TRUE, row_unit = row_unit, 
        col_unit = col_unit, z_unit = z_unit)
    plotsome(T = Td_noise, signal = Td$smat, images = FALSE, 
        indiv = TRUE, of = TRUE, errb = TRUE, stderror = TRUE, 
        row_unit = row_unit, col_unit = col_unit, z_unit = z_unit)
    plotsome(T = Td_noise, signal = Td$smat, transpose = TRUE, 
        images = FALSE, indiv = TRUE, of = TRUE, errb = TRUE, 
        stderror = TRUE, row_unit = row_unit, col_unit = col_unit, 
        z_unit = z_unit)
    plotsome(T = Td_noise, signal = Td$smat, transpose = TRUE, 
        images = FALSE, columns = TRUE, of = TRUE, lf = TRUE, 
        row_unit = row_unit, col_unit = col_unit, z_unit = z_unit)
    plotsome(T = Td_noise, signal = Td$smat, images = FALSE, 
        columns = TRUE, of = TRUE, lf = TRUE, row_unit = row_unit, 
        col_unit = col_unit, z_unit = z_unit)
    stats_residuals(Td_noise, Td = Td, Td_old = Td_old, genname = "Equitable", 
        ylim = NULL, ipf = FALSE, pf = FALSE)
    inc <- round(ncol(Td_noise$smat)/10)
    if (inc < 1) 
        inc <- 1
    nc <- seq(1, ncol(Td_noise$smat), by = inc)
    la <- which(colnames(Td_noise$l.s.pslope) == "Row_Ave")
    if (length(la) != 0) {
        pm <- colMeans(Td_noise$l.s.pslope[, -la], na.rm = TRUE)
    }
    else pm <- colMeans(Td_noise$l.s.pslope, na.rm = TRUE)
    refer <- which(pm == min(pm, na.rm = TRUE))
    if (length(refer) > 0) 
        cat("\nbest reference individual is ", colnames(Td_noise$l.s.pslope)[refer], 
            "\n")
    else {
        cat("\n no minp reference found: reset to 1\n")
        refer <- 1
    }
    if (length(refer) > 1) 
        refer <- refer[1]
    xvsrefplot(Td = Td_noise, cgroup = nc, ref = refer, br = paste0("Equitable Profiles with Min Probability Profile"))
    main <- paste("minp reference\n", colnames(Td_noise$l.s.pslope)[refer])
    plot_hist(Td_noise, refer = refer, main = main)
    if (length(la) == 1) 
        xvsrefplot(Td = Td_noise, cgroup = nc, ref = "Row_Ave", 
            br = "Td_noise")
    if (fintersect) {
        findinfo(Tdave = Td_noise, printmax = FALSE, numb = 1)
    }
    if (fpca) {
        if (mean(Td_noise$l.s.r2, na.rm = TRUE) < 0.95) {
            r2 <- Td_noise$l.s.r2
            removec <- unlist(sapply(1:ncol(r2), function(c) {
                if (length(r2[is.na(r2[, c]), c]) == nrow(r2)) 
                  return(c)
            }))
            if (!is.null(removec)) 
                r2 <- r2[-removec, -removec]
            calc_pca(x = r2, main = "PCA on R2")
        }
        else cat("\nNo pca for r2 on Original Data: Perfect fit\n")
        smat <- Td_noise$smat
        removec <- unlist(sapply(1:ncol(smat), function(c) {
            if (length(smat[is.na(smat[, c]), c]) >= 0.6 * nrow(smat)) 
                return(c)
        }))
        if (!is.null(removec)) 
            smat <- smat[-removec, -removec]
        calc_pca(x = smat, main = "PCA on Original Data")
        ETx <- Td_noise$ET.x
        removec <- unlist(sapply(1:ncol(ETx), function(c) {
            if (length(ETx[is.na(ETx[, c]), c]) >= 0.6 * nrow(ETx)) 
                return(c)
        }))
        if (!is.null(removec)) 
            ETx <- ETx[-removec, -removec]
        calc_pca(x = ETx, main = "PCA on Equitable Transform")
    }
    if (fintersect) {
        if (length(la) != 0) 
            bestintersectname <- a_b_bagplot(community.f = NULL, 
                Td = Td_noise, refindex = la)
        else bestintersectname <- a_b_bagplot(community.f = NULL, 
            Td = Td_noise, refindex = 1)
    }
    if (fave) {
        plotAveprofiles(Td96 = Td_noise, main = "Td_Noise Data")
        if (!is.null(Td)) 
            plotAveprofiles(Td96 = Td, main = "Td Signal Data", 
                xlim = row_unit, ylim = col_unit)
    }
    plotsome(Td_noise, images = FALSE, xvsref = ncol(Td_noise$smat), 
        row_unit = row_unit, col_unit = col_unit, z_unit = z_unit)
    plotsome(T = Td, images = FALSE, xvsref = ncol(Td$smat), 
        row_unit = row_unit, col_unit = col_unit, z_unit = z_unit)
    plotsome(T = Td_noise, signal = Td$smat, images = FALSE, 
        versus = TRUE, of = TRUE, lf = TRUE, row_unit = row_unit, 
        col_unit = col_unit, z_unit = z_unit)
    plotsome(T = Td_noise, signal = Td$smat, of = TRUE, lf = TRUE, 
        errb = TRUE, row_unit = row_unit, col_unit = col_unit, 
        z_unit = z_unit, yma = yma, yline = yline)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
