\name{plotsum4}
\alias{plotsum4}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot all four transforms
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotsum4(T4, row_unit = NULL, col_unit = NULL, z_unit = NULL, fpca = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{T4}{
%%     ~~Describe \code{T4} here~~
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
  \item{fpca}{
%%     ~~Describe \code{fpca} here~~
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
function (T4, row_unit = NULL, col_unit = NULL, z_unit = NULL, 
    fpca = FALSE) 
{
    cmult <- T4$cmult
    rmult <- T4$rmult
    fac <- T4$fac
    NAfrac <- T4$NAfrac
    Td <- T4$Td
    TdNA <- T4$TdNA
    Tdn <- T4$Tdn
    TdnNA <- T4$TdnNA
    if (!is.null(TdnNA)) {
        plotsome(TdnNA, images = FALSE, signal = Td$smat, indiv = TRUE, 
            of = TRUE, errb = TRUE, row_unit = row_unit, col_unit = col_unit, 
            z_unit = z_unit)
        plotsome(TdnNA, images = FALSE, signal = Td$smat, transpose = TRUE, 
            indiv = TRUE, of = TRUE, errb = TRUE, row_unit = row_unit, 
            col_unit = col_unit, z_unit = z_unit)
        plotsome(TdnNA, signal = Td$smat, images = FALSE, indiv = TRUE, 
            of = TRUE, errb = TRUE, stderror = TRUE, row_unit = row_unit, 
            col_unit = col_unit, z_unit = z_unit)
        plotsome(TdnNA, signal = Td$smat, transpose = TRUE, images = FALSE, 
            indiv = TRUE, of = TRUE, errb = TRUE, stderror = TRUE, 
            row_unit = row_unit, col_unit = col_unit, z_unit = z_unit)
        plotsome(TdnNA, signal = Td$smat, images = FALSE, versus = TRUE, 
            of = TRUE, lf = TRUE, row_unit = row_unit, col_unit = col_unit, 
            z_unit = z_unit)
        plotsome(TdnNA, signal = Td$smat, images = TRUE, of = TRUE, 
            lf = TRUE, row_unit = row_unit, col_unit = col_unit, 
            z_unit = z_unit)
        stats_residuals(TdnNA, Td = Td, genname = "Equitable", 
            ylim = NULL, ipf = FALSE, pf = FALSE)
        inc <- round(ncol(TdnNA$smat)/10)
        if (inc < 1) 
            inc <- 1
        nc <- seq(1, ncol(TdnNA$smat), by = inc)
        la <- which(colnames(TdnNA$l.s.pslope) == "Row_Ave")
        if (length(la) != 0) {
            pm <- colMeans(TdnNA$l.s.pslope[, -la], na.rm = TRUE)
        }
        else pm <- colMeans(TdnNA$l.s.pslope, na.rm = TRUE)
        refer <- which(pm == min(pm))
        cat("\nbest reference individual is ", colnames(TdnNA$l.s.pslope)[refer], 
            "\n")
        if (length(refer) > 1) 
            refer <- refer[1]
        xvsrefplot(Td = Tdn, cgroup = nc, ref = refer, br = paste0("Equitable Profiles with Min Probability Profile"))
        main <- paste("minp reference\n", colnames(TdnNA$l.s.pslope)[refer])
        plot_hist(TdnNA, refer = refer, main = main)
        main <- paste("\nEntire Matrix")
        plot_hist(TdnNA, main = main)
        if (NAfrac < 0.76) {
            findinfo(Tdave = TdnNA, printmax = FALSE, numb = 1)
            bestintersectname <- a_b_bagplot(community.f = NULL, 
                Td = TdnNA, refindex = 1, main = "TdnNA")
        }
        if (fpca) {
            calc_pca(x = TdnNA$smat, main = "PCA on Original Data")
        }
    }
    else {
        plotsome(Tdn, images = FALSE, signal = Td$smat, indiv = TRUE, 
            of = TRUE, errb = TRUE, row_unit = row_unit, col_unit = col_unit, 
            z_unit = z_unit)
        plotsome(Tdn, images = FALSE, signal = Td$smat, transpose = TRUE, 
            indiv = TRUE, of = TRUE, errb = TRUE, row_unit = row_unit, 
            col_unit = col_unit, z_unit = z_unit)
        plotsome(Tdn, signal = Td$smat, images = FALSE, indiv = TRUE, 
            of = TRUE, errb = TRUE, stderror = TRUE, row_unit = row_unit, 
            col_unit = col_unit, z_unit = z_unit)
        plotsome(Tdn, signal = Td$smat, transpose = TRUE, images = FALSE, 
            indiv = TRUE, of = TRUE, errb = TRUE, stderror = TRUE, 
            row_unit = row_unit, col_unit = col_unit, z_unit = z_unit)
        plotsome(Tdn, signal = Td$smat, images = FALSE, versus = TRUE, 
            of = TRUE, lf = TRUE, row_unit = row_unit, col_unit = col_unit, 
            z_unit = z_unit)
        inc <- round(ncol(Tdn$smat)/10)
        if (inc < 1) 
            inc <- 1
        nc <- seq(1, ncol(Tdn$smat), by = inc)
        la <- which(colnames(Tdn$l.s.pslope) == "Row_Ave")
        if (length(la) != 0) {
            pm <- colMeans(Tdn$l.s.pslope[, -la], na.rm = TRUE)
        }
        else pm <- colMeans(Tdn$l.s.pslope, na.rm = TRUE)
        refer <- which(pm == min(pm))
        cat("\nbest reference individual is ", colnames(Tdn$l.s.pslope)[refer], 
            "\n")
        if (length(refer) > 1) 
            refer <- refer[1]
        xvsrefplot(Td = Tdn, cgroup = nc, ref = refer, br = paste0("Equitable Profiles with Min Probability Profile"))
        main <- paste("minp reference\n", colnames(Tdn$l.s.pslope)[refer])
        plot_hist(Tdn, refer = refer, main = main)
        main <- paste("\nEntire Matrix")
        plot_hist(Tdn, main = main)
        findinfo(Tdave = Tdn, printmax = FALSE, numb = 1)
        bestintersectname <- a_b_bagplot(community.f = NULL, 
            Td = Tdn, refindex = 1, main = "Tdn")
        if (mean(Tdn$l.s.r2, na.rm = TRUE) < 0.97) {
            calc_pca(x = Tdn$smat, main = "PCA on Original Data")
            calc_pca(x = Tdn$ET.x, main = "PCA on Equitable Transform")
        }
    }
    if (!is.null(TdnNA$ET.x)) 
        imagenan(TdnNA$ET.x, main = paste0("ET from Signal+Noise with NAs : rmult= ", 
            rmult, " cmult= ", cmult, "\nfac=", fac, " fraction of NA=", 
            NAfrac), row_unit = row_unit, col_unit = col_unit, 
            zlim = z_unit)
    if (!is.null(TdnNA$smat)) 
        imagenan(TdnNA$smat, main = paste0("Signal+Noise with NAs : rmult= ", 
            rmult, " cmult= ", cmult, "\nfac=", fac, " fraction of NA=", 
            NAfrac), row_unit = row_unit, col_unit = col_unit, 
            zlim = z_unit)
    if (!is.null(TdNA$smat)) 
        imagenan(TdNA$ET.x, main = paste0("ET from Signal with NAs: rmult= ", 
            rmult, " cmult= ", cmult, " fraction of NA=", NAfrac), 
            row_unit = row_unit, col_unit = col_unit, zlim = z_unit)
    if (!is.null(TdNA$smat)) 
        imagenan(TdNA$smat, main = paste0("Signal with NAs: rmult= ", 
            rmult, " cmult= ", cmult, " fraction of NA=", NAfrac), 
            row_unit = row_unit, col_unit = col_unit, zlim = z_unit)
    imagenan(Tdn$ET.x, main = paste0("ET from Signal+Noise: rmult= ", 
        rmult, " cmult= ", cmult, "\nfac=", fac), row_unit = row_unit, 
        col_unit = col_unit, zlim = z_unit)
    imagenan(Tdn$smat, main = paste0("Signal+Noise: rmult= ", 
        rmult, " cmult= ", cmult, "\nfac=", fac), row_unit = row_unit, 
        col_unit = col_unit, zlim = z_unit)
    if (!is.null(Td$smat)) 
        imagenan(Td$smat, main = paste0("Signal : rmult= ", rmult, 
            " cmult= ", cmult), row_unit = row_unit, col_unit = col_unit, 
            zlim = z_unit)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
