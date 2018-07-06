\name{plotO_S_E_lscol}
\alias{plotO_S_E_lscol}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot  columns
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotO_S_E_lscol(orig, E, ls, x_unit = NULL, y_unit = NULL, signal = NULL, Eave, ylimits = NULL, xlimits = NULL, genname = NULL, of = FALSE, lf = FALSE, ef = TRUE, avef = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{orig}{
%%     ~~Describe \code{orig} here~~
}
  \item{E}{
%%     ~~Describe \code{E} here~~
}
  \item{ls}{
%%     ~~Describe \code{ls} here~~
}
  \item{x_unit}{
%%     ~~Describe \code{x_unit} here~~
}
  \item{y_unit}{
%%     ~~Describe \code{y_unit} here~~
}
  \item{signal}{
%%     ~~Describe \code{signal} here~~
}
  \item{Eave}{
%%     ~~Describe \code{Eave} here~~
}
  \item{ylimits}{
%%     ~~Describe \code{ylimits} here~~
}
  \item{xlimits}{
%%     ~~Describe \code{xlimits} here~~
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
function (orig, E, ls, x_unit = NULL, y_unit = NULL, signal = NULL, 
    Eave, ylimits = NULL, xlimits = NULL, genname = NULL, of = FALSE, 
    lf = FALSE, ef = TRUE, avef = FALSE) 
{
    if (is.null(genname)) 
        genname <- "Data"
    if (is.null(ylimits)) 
        ylimits <- c(min(E, na.rm = TRUE), max(E, na.rm = TRUE))
    if (!is.infinite(ylimits[1])) {
        if (is.null(xlimits)) 
            xlimits <- c(1, nrow(E))
        if (of) 
            plot_columns(orig, main = paste(" Noise: Original", 
                genname), xlimits = xlimits, limits = ylimits, 
                x_unit = x_unit, y_unit = y_unit)
        if (!is.null(signal)) 
            plot_columns(signal, main = paste("No Noise: Signal ", 
                genname), xlimits = xlimits, limits = ylimits, 
                x_unit = x_unit, y_unit = y_unit)
        if (ef) 
            plot_columns(E, main = paste("Noise: Equitable Transform", 
                genname), xlimits = xlimits, limits = ylimits, 
                x_unit = x_unit, y_unit = y_unit)
        if (avef) 
            plot_columns(Eave, main = paste("Noise: Equitable Transform REFERENCED", 
                genname), xlab = "(Row  index)", xlimits = xlimits, 
                limits = ylimits, x_unit = x_unit, y_unit = y_unit)
        if (lf) 
            plot_columns(ls, main = paste("Noise: Least Squared Transform", 
                genname), xlimits = xlimits, limits = ylimits, 
                x_unit = x_unit, y_unit = y_unit)
        if (!is.null(signal)) 
            plot_columns(signal, main = "No Noise: Signal", xlimits = xlimits, 
                limits = ylimits, x_unit = x_unit, y_unit = y_unit)
    }
    else cat("\nplotO_S_E_lscol : ERROR: transform is all Nan")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
