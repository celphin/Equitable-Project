\name{plotsquares}
\alias{plotsquares}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot quare matrix
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotsquares(Ta, num = 5, signal = NULL, xlimits = NULL, slimits = NULL, blimits = NULL, indiv = FALSE, columns = FALSE, images = TRUE, transpose = FALSE, density = FALSE, main = "", psf = TRUE, of = FALSE, lf = FALSE, ef = TRUE, errb = FALSE, row_unit = NULL, col_unit = NULL, z_unit = NULL, yline = 3, yma = 5, stderror = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ta}{
%%     ~~Describe \code{Ta} here~~
}
  \item{num}{
%%     ~~Describe \code{num} here~~
}
  \item{signal}{
%%     ~~Describe \code{signal} here~~
}
  \item{xlimits}{
%%     ~~Describe \code{xlimits} here~~
}
  \item{slimits}{
%%     ~~Describe \code{slimits} here~~
}
  \item{blimits}{
%%     ~~Describe \code{blimits} here~~
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
  \item{transpose}{
%%     ~~Describe \code{transpose} here~~
}
  \item{density}{
%%     ~~Describe \code{density} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
}
  \item{psf}{
%%     ~~Describe \code{psf} here~~
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
  \item{yline}{
%%     ~~Describe \code{yline} here~~
}
  \item{yma}{
%%     ~~Describe \code{yma} here~~
}
  \item{stderror}{
%%     ~~Describe \code{stderror} here~~
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
function (Ta, num = 5, signal = NULL, xlimits = NULL, slimits = NULL, 
    blimits = NULL, indiv = FALSE, columns = FALSE, images = TRUE, 
    transpose = FALSE, density = FALSE, main = "", psf = TRUE, 
    of = FALSE, lf = FALSE, ef = TRUE, errb = FALSE, row_unit = NULL, 
    col_unit = NULL, z_unit = NULL, yline = 3, yma = 5, stderror = FALSE) 
{
    if (is.null(z_unit)) {
        z_unit <- "Data Value"
        bz_unit <- paste("Intercept")
    }
    else {
        bz_unit <- paste("Intercept", z_unit)
        z_unit <- paste("Slope", z_unit)
    }
    row_unit = col_unit
    if (!is.null(signal)) {
        orig <- signal$l.s.s
        origb <- signal$l.s.b
    }
    else {
        orig <- Ta$l.s.s
        origb <- Ta$l.s.b
    }
    Eerr = NULL
    lserr = NULL
    Eaveerr = NULL
    Eerrb = NULL
    lserrb = NULL
    Eaveerrb = NULL
    if (transpose) {
        o <- t(orig)
        E <- t(Ta$ET.Es)
        ls <- t(Ta$l.s.s)
        ge <- "Transposed "
        ob <- t(origb)
        Eb <- t(Ta$ET.Eb)
        lb <- t(Ta$l.s.b)
        if (!is.null(col_unit)) 
            y_unit <- paste("MATRIX ROWS ", col_unit)
        else y_unit <- "ROWS"
        if (!is.null(row_unit)) 
            x_unit <- paste("MATRIX COLUMNS ", row_unit)
        else x_unit <- "COLUMNS"
        if (errb) {
            Eerr = t(Ta$E.sd1)
            lserr = t(Ta$l.s.sse)
            Eerrb = t(Ta$E.bsd1)
            lserrb = t(Ta$l.s.bse)
            Eaveerr = NULL
            if (stderror) {
                Eerr <- Eerr/sqrt(t(Ta$E.sN))
                ge <- paste0(g, " std. error of Mean")
            }
        }
    }
    else {
        o <- orig
        E <- Ta$ET.Es
        ls <- Ta$l.s.s
        ge <- " "
        ob <- (origb)
        Eb <- (Ta$ET.Eb)
        lb <- (Ta$l.s.b)
        if (!is.null(col_unit)) 
            x_unit <- paste("MATRIX ROWS ", col_unit)
        else x_unit <- "ROWS"
        if (!is.null(row_unit)) 
            y_unit <- paste("MATRIX COLUMNS ", row_unit)
        else y_unit <- "COLUMNS"
        if (errb) {
            Eerr = (Ta$E.sd1)
            lserr = (Ta$l.s.sse)
            Eerrb = (Ta$E.bsd1)
            lserrb = (Ta$l.s.bse)
            Eaveerrb = NULL
            if (stderror) {
                Eerr <- Eerr/sqrt((Ta$E.sN))
                Eerrb <- Eerrb/sqrt((Ta$E.bN))
                ge <- paste0(ge, " std. error of Mean")
            }
        }
    }
    if (is.null(slimits)) {
        if (errb) 
            szlimits <- c(mean(c(E - Eerr), na.rm = TRUE) - 4 * 
                sd(c(E - Eerr), na.rm = TRUE), mean(c(E + Eerr), 
                na.rm = TRUE) + 4 * sd(c(E + Eerr), na.rm = TRUE))
        else szlimits <- c(mean(c(E), na.rm = TRUE) - 4 * sd(c(E), 
            na.rm = TRUE), mean(c(E), na.rm = TRUE) + 4 * sd(c(E), 
            na.rm = TRUE))
        iszlimits <- c(0, 2)
    }
    else {
        szlimits <- slimits
        iszlimits <- slimits
    }
    if (is.null(blimits)) {
        if (errb) 
            bzlimits <- c(mean(c(Eb - Eerrb), na.rm = TRUE) - 
                4 * sd(c(Eb - Eerrb), na.rm = TRUE), mean(c(Eb + 
                Eerrb), na.rm = TRUE) + 4 * sd(c(Eb + Eerrb), 
                na.rm = TRUE))
        else bzlimits <- c(mean(c(Eb), na.rm = TRUE) - 4 * sd(c(Eb), 
            na.rm = TRUE), mean(c(Eb), na.rm = TRUE) + 4 * sd(c(Eb), 
            na.rm = TRUE))
        me <- mean(Eb, na.rm = TRUE)
        if (me < 0) {
            mi <- me
            ma <- (-1) * me
        }
        else {
            mi <- (-1) * me
            ma <- me
        }
        ibzlimits <- c(mi, ma)
        ibzlimits <- c(mean(Eb, na.rm = TRUE) - 1 * sd(Eb, na.rm = TRUE), 
            mean(Eb, na.rm = TRUE) + 1 * sd(Eb, na.rm = TRUE))
    }
    else {
        bzlimits <- blimits
        ibzlimits <- blimits
    }
    if (is.null(xlimits)) 
        xlimits <- c(1, nrow(o))
    if (psf) {
        if (images) 
            plotimages(o, E, ls, zlimits = iszlimits, genname = paste(ge, 
                "Slopes"), of = of, lf = lf, ef = ef, row_unit = y_unit, 
                col_unit = x_unit, yma = yma, yline = yline)
        if (columns) 
            plotO_S_E_lscol(o, E, ls, xlimits = xlimits, ylimits = szlimits, 
                genname = paste(main, ge, "Slopes"), of = of, 
                lf = lf, ef = ef, x_unit = x_unit, y_unit = z_unit)
        if (indiv) {
            plotindivid(o, E, ls, xlimits = xlimits, ylimits = szlimits, 
                num = num, genname = paste(main, ge, "Slopes"), 
                of = of, lf = lf, ef = ef, err = errb, Eerr = Eerr, 
                lserr = lserr, Eaveerr = Eaveerr, x_unit = x_unit, 
                y_unit = z_unit)
            vv <- seq(xlimits[1], xlimits[2], by = (xlimits[2] - 
                xlimits[1])/100)
            lines(vv, rep(1, length(vv)))
        }
    }
    if (images) 
        plotimages(ob, Eb, lb, zlimits = ibzlimits, genname = paste(ge, 
            "Intercepts"), of = of, lf = lf, ef = ef, row_unit = y_unit, 
            col_unit = x_unit, yma = yma, yline = yline)
    if (columns) 
        plotO_S_E_lscol(ob, Eb, lb, xlimits = xlimits, ylimits = bzlimits, 
            genname = paste(main, ge, "Intercepts"), of = of, 
            lf = lf, ef = ef, x_unit = x_unit, y_unit = bz_unit)
    if (indiv) {
        plotindivid(ob, Eb, lb, xlimits = xlimits, ylimits = bzlimits, 
            num = num, genname = paste(main, ge, "Intercepts"), 
            of = of, lf = lf, ef = ef, err = errb, Eerr = Eerrb, 
            lserr = lserrb, Eaveerr = Eaveerrb, x_unit = x_unit, 
            y_unit = bz_unit)
        vv <- seq(xlimits[1], xlimits[2], by = (xlimits[2] - 
            xlimits[1])/100)
        lines(vv, rep(0, length(vv)))
    }
    if (density) {
        if (of) 
            plotdensity(o, num = num, genname = "Signal Slope")
        if (lf) 
            plotdensity(ls, num = num, genname = "Least Squares Slope")
        if (ef) 
            plotdensity(E, num = num, genname = "Equitable Slope")
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
