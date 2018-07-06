\name{plotimages}
\alias{plotimages}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot some images 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotimages(orig, E, ls, row_unit = NULL, col_unit = NULL, signal = NULL, Eave, genname = NULL, zlimits = NULL, of = FALSE, lf = FALSE, ef = TRUE, yline = 3, yma = 5, avef = FALSE, fcontour = TRUE)
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
  \item{row_unit}{
%%     ~~Describe \code{row_unit} here~~
}
  \item{col_unit}{
%%     ~~Describe \code{col_unit} here~~
}
  \item{signal}{
%%     ~~Describe \code{signal} here~~
}
  \item{Eave}{
%%     ~~Describe \code{Eave} here~~
}
  \item{genname}{
%%     ~~Describe \code{genname} here~~
}
  \item{zlimits}{
%%     ~~Describe \code{zlimits} here~~
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
  \item{yline}{
%%     ~~Describe \code{yline} here~~
}
  \item{yma}{
%%     ~~Describe \code{yma} here~~
}
  \item{avef}{
%%     ~~Describe \code{avef} here~~
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
function (orig, E, ls, row_unit = NULL, col_unit = NULL, signal = NULL, 
    Eave, genname = NULL, zlimits = NULL, of = FALSE, lf = FALSE, 
    ef = TRUE, yline = 3, yma = 5, avef = FALSE, fcontour = TRUE) 
{
    if (is.null(zlimits)) 
        zlimits <- c(min(E, na.rm = TRUE), max(E, na.rm = TRUE))
    if (!is.infinite(zlimits[1])) {
        if (zlimits[2] <= zlimits[1] + 1e-06) 
            zlimits <- c(zlimits[1] - 1/10, zlimits[1] + 1/10)
        if (is.null(genname)) 
            genname <- "Data"
        if (fcontour) {
            if (of && sd(orig, na.rm = TRUE) > 1e-06) 
                contour(orig, main = paste0("Original", genname), 
                  zlim = zlimits/1, xlab = row_unit, ylab = col_unit)
            if (!is.null(signal) && sd(signal, na.rm = TRUE) > 
                1e-06) 
                contour(signal, main = paste0("No Noise Signal: ", 
                  genname), zlim = zlimits/1, xlab = row_unit, 
                  ylab = col_unit)
            if (ef && sd(E, na.rm = TRUE) > 1e-06) 
                contour(E, main = paste0("T Noise Equitable", 
                  genname), zlim = zlimits/1, xlab = row_unit, 
                  ylab = col_unit)
            if (avef && sd(Eave, na.rm = TRUE) > 1e-06) 
                contour(Eave, main = paste0("T Noise Equitable REFERENCED ", 
                  genname), zlim = zlimits/1, xlab = row_unit, 
                  ylab = col_unit)
            if (lf && sd(ls, na.rm = TRUE) > 1e-06) 
                contour(ls, main = paste0("T Noise Least squared: ", 
                  genname), zlim = zlimits/1, xlab = row_unit, 
                  ylab = col_unit)
        }
        if (of) 
            imagenan(orig, main = paste0("Original: ", genname), 
                zlim = zlimits/1, row_unit = row_unit, col_unit = col_unit, 
                yma = yma, yline = yline)
        if (!is.null(signal)) 
            imagenan(signal, main = paste0("No Noise Signal: ", 
                genname), zlim = zlimits/1, row_unit = row_unit, 
                col_unit = col_unit)
        if (ef) 
            imagenan(E, main = paste0("T Noise Equitable : ", 
                genname), zlim = zlimits/1, row_unit = row_unit, 
                col_unit = col_unit, yma = yma, yline = yline)
        if (avef) 
            imagenan(Eave, main = paste0("T Noise Equitable REFERENCED : ", 
                genname), zlim = zlimits/1, row_unit = row_unit, 
                col_unit = col_unit, yma = yma, yline = yline)
        if (lf) 
            imagenan(ls, main = paste0("T Noise Least squared: ", 
                genname), zlim = zlimits/1, row_unit = row_unit, 
                col_unit = col_unit, yma = yma, yline = yline)
    }
    else cat("\n ERROR: images transform is all nan")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
