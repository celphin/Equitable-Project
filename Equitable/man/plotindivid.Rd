\name{plotindivid}
\alias{plotindivid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot individuals
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotindivid(I, zw, lsz, num = NULL, signal = NULL, Eave = NULL, xlimits = NULL, ylimits = NULL, genname = NULL, of = FALSE, lf = FALSE, ef = TRUE, avef = FALSE, err = FALSE, Eerr = NULL, lserr = NULL, x_unit = NULL, y_unit = NULL, Eaveerr = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{I}{
%%     ~~Describe \code{I} here~~
}
  \item{zw}{
%%     ~~Describe \code{zw} here~~
}
  \item{lsz}{
%%     ~~Describe \code{lsz} here~~
}
  \item{num}{
%%     ~~Describe \code{num} here~~
}
  \item{signal}{
%%     ~~Describe \code{signal} here~~
}
  \item{Eave}{
%%     ~~Describe \code{Eave} here~~
}
  \item{xlimits}{
%%     ~~Describe \code{xlimits} here~~
}
  \item{ylimits}{
%%     ~~Describe \code{ylimits} here~~
}
  \item{genname}{
%%     ~~Describe \code{genname} here~~
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
  \item{err}{
%%     ~~Describe \code{err} here~~
}
  \item{Eerr}{
%%     ~~Describe \code{Eerr} here~~
}
  \item{lserr}{
%%     ~~Describe \code{lserr} here~~
}
  \item{x_unit}{
%%     ~~Describe \code{x_unit} here~~
}
  \item{y_unit}{
%%     ~~Describe \code{y_unit} here~~
}
  \item{Eaveerr}{
%%     ~~Describe \code{Eaveerr} here~~
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
function (I, zw, lsz, num = NULL, signal = NULL, Eave = NULL, 
    xlimits = NULL, ylimits = NULL, genname = NULL, of = FALSE, 
    lf = FALSE, ef = TRUE, avef = FALSE, err = FALSE, Eerr = NULL, 
    lserr = NULL, x_unit = NULL, y_unit = NULL, Eaveerr = NULL) 
{
    if (is.null(ylimits)) 
        ylimits <- c(min(zw, na.rm = TRUE)/1, max(zw, na.rm = TRUE) * 
            1)
    if (is.null(xlimits)) 
        xlimits <- c(1, nrow(zw))
    if (is.null(num)) 
        num <- (ncol(I) - 1)
    if (length(num) == 1) 
        listx <- seq(1, ncol(I), by = (ncol(I) - 1)/num)
    else {
        if (length(num) == 2 && num[1] == num[2]) 
            listx <- c(num[1])
        else listx <- num
    }
    rnames <- rownames(I)
    for (y in listx) {
        y <- floor(y)
        yn <- colnames(I)[y]
        if (ef) {
            if (err) {
                plotdata_with_errors(zw[, y], Eerr[, y], xlab = x_unit, 
                  ylab = paste(y_unit, yn), rnames = rnames, 
                  xlim = xlimits, main = paste("Index shown=", 
                    yn, "\nIncludes Error", genname), cex.main = 0.7, 
                  ylim = ylimits, pch = 11)
            }
            else {
                plot(zw[, y], xlab = x_unit, ylab = paste(y_unit, 
                  yn), xlim = xlimits, ylim = ylimits, type = "p", 
                  pch = 11, main = paste("Index shown ", yn, 
                    "\n", genname), cex.main = 0.7)
            }
        }
        else {
            if (of && !lf && !ef && !avef) {
                plot(I[, y], xlab = x_unit, ylab = paste(y_unit, 
                  yn), main = paste("Index shown ", yn, "\n", 
                  genname), cex.main = 0.7, xlim = xlimits, ylim = ylimits, 
                  type = "p", pch = 15)
            }
            else {
                if (lf) {
                  plotdata_with_errors(lsz[, y], lserr[, y], 
                    xlab = x_unit, ylab = paste(y_unit, yn), 
                    rnames = rnames, xlim = xlimits, main = paste("Index shown ", 
                      yn, "\n", genname), cex.main = 0.7, ylim = ylimits, 
                    pch = 1)
                }
                else {
                  plotdata_with_errors(Eave[, y], Eaveerr[, y], 
                    xlab = x_unit, ylab = paste(y_unit, yn), 
                    rnames = rnames, main = paste("Index shown ", 
                      yn, "\n", genname), cex.main = 0.7, xlim = xlimits, 
                    ylim = ylimits, pch = 0)
                }
            }
        }
        pch <- t((c(NA, NA, NA, NA, NA)))
        legend <- c(NA, NA, NA, NA, NA)
        lwd <- c(NA, NA, NA, NA, NA)
        if (!is.null(signal)) {
            legend[5] <- "Signal"
            lwd[5] <- 4
        }
        if (of) {
            legend[1] <- "Original"
            pch[1] <- 15
        }
        if (ef) {
            legend[2] <- "Equitable"
            pch[2] <- 11
        }
        if (lf) {
            legend[3] <- "Least Squared"
            pch[3] <- 1
        }
        if (avef) {
            legend[4] <- "Eq. from Ave col"
            pch[4] <- 0
        }
        legend("topleft", inset = 0.02, legend = legend, lwd = lwd, 
            pch = pch, bg = "white", ncol = c(2), cex = 0.75)
        if (of) 
            lines(I[, y], type = "p", pch = 15)
        if (!is.null(signal)) 
            lines(signal[, y], type = "l", pch = 15, lwd = 4)
        if (lf) 
            lines(lsz[, y], type = "p", pch = 1, lwd = 2)
        if (avef) 
            lines(Eave[, y], type = "p", pch = 0, lwd = 2)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
