\name{imagenan}
\alias{imagenan}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
make image: modifed from web image function
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
imagenan(x, yline = 3, yma = 5, col = topo.colors(255), outside.below.color = "black", outside.above.color = "white", na.color = "gray", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{yline}{
%%     ~~Describe \code{yline} here~~
}
  \item{yma}{
%%     ~~Describe \code{yma} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{outside.below.color}{
%%     ~~Describe \code{outside.below.color} here~~
}
  \item{outside.above.color}{
%%     ~~Describe \code{outside.above.color} here~~
}
  \item{na.color}{
%%     ~~Describe \code{na.color} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (x, yline = 3, yma = 5, col = topo.colors(255), outside.below.color = "black", 
    outside.above.color = "white", na.color = "gray", ...) 
{
    x <- as.matrix(x)
    .pardefault <- par(no.readonly = TRUE)
    reverse <- nrow(x):1
    x <- x[reverse, ]
    zlim = c(min(x, na.rm = TRUE), max(x, na.rm = TRUE))
    if (zlim[2] <= zlim[1] + 1e-10) 
        zlim <- c(zlim[1], zlim[1] + 1/100)
    if (!is.null(rownames(x))) 
        rLabels <- rownames(x)
    else {
        rLabels <- c(reverse)
        rownames(x) <- c(reverse)
    }
    cLabels <- colnames(x)
    er <- nrow(x)
    ec <- ncol(x)
    main <- " "
    row_unit <- "Rows"
    col_unit <- "Columns"
    zunit <- "Intensity"
    rtick = 1
    ctick = 1
    rtickinc = round(er/10)
    ctickinc = round(ec/10)
    if (rtickinc == 0) 
        rtickinc = 1
    if (ctickinc == 0) 
        ctickinc = 1
    rnames <- rownames(x)
    cnames <- colnames(x)
    attr = c(seq(rtick, er, by = rtickinc))
    attc = c(seq(ctick, ec, by = ctickinc))
    rlbls = rnames[seq(rtick, er, by = rtickinc)]
    clbls = cnames[seq(ctick, ec, by = ctickinc)]
    if (length(list(...))) {
        Lst <- list(...)
        if (!is.null(Lst$zlim)) {
            zlim <- c(Lst$zlim)
            if (zlim[2] <= zlim[1]) 
                zlim <- c(zlim[1], zlim[1] + 1/100)
        }
        if (!is.null(Lst$yLabels)) {
            cLabels <- c(Lst$cLabels)
        }
        if (!is.null(Lst$xLabels)) {
            rLabels <- c(Lst$rLabels)
        }
        if (!is.null(Lst$main)) {
            main <- Lst$main
        }
        if (!is.null(Lst$row_unit)) {
            row_unit <- Lst$row_unit
        }
        if (!is.null(Lst$col_unit)) {
            col_unit <- Lst$col_unit
        }
        if (!is.null(Lst$rlbls)) {
            rlbls <- Lst$rlbls
        }
        if (!is.null(Lst$clbls)) {
            clbls <- Lst$clbls
        }
        if (!is.null(Lst$zunit)) {
            zunit <- Lst$zunit
        }
    }
    if (is.null(rLabels)) {
        rLabels <- c(reverse)
    }
    if (is.null(cLabels)) {
        cLabels <- c(1:ncol(x))
    }
    layout(matrix(data = c(1, 1, 2, 3), nrow = 2, ncol = 2), 
        widths = c(5, 1.25), heights = c(1, 0.5))
    zstep <- (zlim[2] - zlim[1])/length(col)
    newz.below.outside <- zlim[1] - 2 * zstep
    newz.above.outside <- zlim[2] + zstep
    newz.na <- zlim[2] + 2 * zstep
    x[which(x < zlim[1])] <- newz.below.outside
    x[which(x > zlim[2])] <- newz.above.outside
    x[which(is.na(x > zlim[2]))] <- newz.na
    zlim[1] <- zlim[1] - 2 * zstep
    zlim[2] <- zlim[2] + 2 * zstep
    col <- c(outside.below.color, col[1], col, outside.above.color, 
        na.color)
    ColorRamp <- col
    if (zlim[1] >= zlim[2]) 
        zlim[2] <- zlim[1] + 0.01 * zlim[1]
    ColorLevels <- seq(zlim[1], zlim[2], length = 100)
    par(mar = c(5, 4, 4, 2))
    par(mar = c(6, yma, 4, 2))
    image(1:length(cLabels), 1:length(rLabels), t(x), col = ColorRamp, 
        xlab = col_unit, main = main, ylab = "", axes = FALSE, 
        zlim = zlim)
    mtext(row_unit, side = 2, line = yline)
    if (!is.null(main)) {
        title(main = main)
    }
    axis(BELOW <- 1, at = attc, labels = clbls, cex.axis = 0.7)
    axis(LEFT <- 2, at = attr, labels = rlbls, las = HORIZONTAL <- 1, 
        cex.axis = 0.7)
    par(mar = c(3, 1, 2.5, 4))
    image(1, ColorLevels, matrix(data = ColorLevels[1:length(ColorLevels) - 
        1], ncol = length(ColorLevels) - 1), col = ColorRamp, 
        xlab = "", ylab = zunit, xaxt = "n")
    par(.pardefault)
    layout(1)
    par(mfrow = c(1, 1))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
