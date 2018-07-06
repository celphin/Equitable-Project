\name{plotsome}
\alias{plotsome}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot some data
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotsome(T, num = 5, signal = NULL, limits = NULL, xlimits = NULL, indiv = FALSE, columns = FALSE, images = TRUE, density = FALSE, versus = FALSE, xvsref = NULL, yline = 3, yma = 5, transpose = FALSE, of = FALSE, lf = FALSE, ef = TRUE, avef = FALSE, errb = FALSE, row_unit = NULL, col_unit = NULL, z_unit = NULL, genname = NULL, stderror = FALSE, fcontour = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{T}{
%%     ~~Describe \code{T} here~~
}
  \item{num}{
%%     ~~Describe \code{num} here~~
}
  \item{signal}{
%%     ~~Describe \code{signal} here~~
}
  \item{limits}{
%%     ~~Describe \code{limits} here~~
}
  \item{xlimits}{
%%     ~~Describe \code{xlimits} here~~
}
  \item{indiv}{
%%     ~~Describe \code{indiv} here~~
}
  \item{columns}{
%%     ~~Describe \code{columns} here~~
}
  \item{images}{
%%     ~~Describe \code{images} here~~
}
  \item{density}{
%%     ~~Describe \code{density} here~~
}
  \item{versus}{
%%     ~~Describe \code{versus} here~~
}
  \item{xvsref}{
%%     ~~Describe \code{xvsref} here~~
}
  \item{yline}{
%%     ~~Describe \code{yline} here~~
}
  \item{yma}{
%%     ~~Describe \code{yma} here~~
}
  \item{transpose}{
%%     ~~Describe \code{transpose} here~~
}
  \item{of}{
%%     ~~Describe \code{of} here~~
}
  \item{lf}{
%%     ~~Describe \code{lf} here~~
}
  \item{ef}{
%%     ~~Describe \code{ef} here~~
}
  \item{avef}{
%%     ~~Describe \code{avef} here~~
}
  \item{errb}{
%%     ~~Describe \code{errb} here~~
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
  \item{genname}{
%%     ~~Describe \code{genname} here~~
}
  \item{stderror}{
%%     ~~Describe \code{stderror} here~~
}
  \item{fcontour}{
%%     ~~Describe \code{fcontour} here~~
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
function (T, num = 5, signal = NULL, limits = NULL, xlimits = NULL, 
    indiv = FALSE, columns = FALSE, images = TRUE, density = FALSE, 
    versus = FALSE, xvsref = NULL, yline = 3, yma = 5, transpose = FALSE, 
    of = FALSE, lf = FALSE, ef = TRUE, avef = FALSE, errb = FALSE, 
    row_unit = NULL, col_unit = NULL, z_unit = NULL, genname = NULL, 
    stderror = FALSE, fcontour = TRUE) 
{
    if (is.null(z_unit)) 
        z_unit <- "Data Value"
    if (transpose) {
        o <- t(T$smat)
        E <- t(T$ET.x)
        ls <- t(T$l.s.x)
        As <- t(T$l.s.x)
        if (!is.null(signal)) 
            signal <- t(signal)
        Eave <- t(T$Ave.ET.x)
        Eerr <- t(T$ET.xsd)
        lserr <- t(T$l.s.xsd)
        Eaveerr <- t(T$Ave.ET.xsd)
        genname <- paste0("Transposed ", genname)
        if (!is.null(col_unit)) 
            y_unit <- col_unit
        else y_unit <- "COLUMNS"
        if (!is.null(row_unit)) 
            x_unit <- row_unit
        else x_unit <- "ROWS"
        if (stderror) {
            Eerr <- Eerr/sqrt(t(T$ET.EN) - 1)
            lserr <- lserr/sqrt(t(T$l.s.EN) - 1)
            Eaveerr <- Eaveerr/sqrt(t(T$Ave.ET.EN) - 1)
            genname <- paste0(genname, " std. error of Mean")
        }
    }
    else {
        o <- T$smat
        E <- T$ET.x
        ls <- T$l.s.x
        signal <- signal
        Eave <- T$Ave.ET.x
        Eerr <- T$ET.xsd
        lserr <- T$l.s.xsd
        Eaveerr <- T$Ave.ET.xsd
        if (!is.null(col_unit)) 
            x_unit <- col_unit
        else x_unit <- "COLUMNS"
        if (!is.null(row_unit)) 
            y_unit <- row_unit
        else y_unit <- "ROWS"
        if (stderror) {
            Eerr <- Eerr/sqrt((T$ET.EN) - 1)
            lserr <- lserr/sqrt((T$l.s.EN) - 1)
            Eaveerr <- Eaveerr/sqrt((T$Ave.ET.EN) - 1)
            genname <- paste0(genname, " std. error of Mean")
        }
    }
    if (images) 
        plotimages(o, E, ls, signal, Eave, genname = genname, 
            of = of, lf = lf, ef = ef, avef = avef, zlimits = limits, 
            row_unit = y_unit, col_unit = x_unit, yma = yma, 
            yline = yline, fcontour = fcontour)
    if (indiv) 
        plotindivid(o, E, ls, num = num, signal, Eave, genname = genname, 
            of = of, lf = lf, ef = ef, avef = avef, err = errb, 
            Eerr = Eerr, lserr = lserr, Eaveerr = Eaveerr, ylimits = limits, 
            xlimits = xlimits, x_unit = y_unit, y_unit = z_unit)
    if (columns) 
        plotO_S_E_lscol(o, E, ls, signal, Eave, genname = genname, 
            of = of, lf = lf, ef = ef, avef = avef, ylimits = limits, 
            xlimits = xlimits, x_unit = y_unit, y_unit = z_unit)
    if (versus) 
        plotversus(o, E, ls, signal, Eave, genname = genname, 
            of = of, lf = lf, ef = ef, avef = avef, ylimits = limits)
    if (!is.null(xvsref)) {
        if (xvsref == "max" || length(which(!is.na(Eave[, xvsref]))) == 
            0) 
            xvsref <- floor(which(abs(Eave) == max(abs(Eave), 
                na.rm = TRUE))/nrow(Eave))
        plot_vsref(Eave, xvsref, main = paste("Equitable Transform\n compared to Column  (Black)", 
            xvsref), lty = "l", limits = limits)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
