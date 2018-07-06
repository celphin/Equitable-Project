\name{rows_via_intercept}
\alias{rows_via_intercept}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
rows via intercept
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rows_via_intercept(Td_noise, rowlist = NULL, pnum = 3, ref = NULL, z_unit = NULL, AVE = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Td_noise}{
%%     ~~Describe \code{Td_noise} here~~
}
  \item{rowlist}{
%%     ~~Describe \code{rowlist} here~~
}
  \item{pnum}{
%%     ~~Describe \code{pnum} here~~
}
  \item{ref}{
%%     ~~Describe \code{ref} here~~
}
  \item{z_unit}{
%%     ~~Describe \code{z_unit} here~~
}
  \item{AVE}{
%%     ~~Describe \code{AVE} here~~
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
function (Td_noise, rowlist = NULL, pnum = 3, ref = NULL, z_unit = NULL, 
    AVE = TRUE) 
{
    if (is.null(rowlist)) 
        rowlist <- seq(nrow(Td_noise$smat), 1, by = (-1 * nrow(Td_noise$smat)/pnum))
    if (is.null(ref)) 
        ref <- ncol(Td_noise$smat)
    sdf <- 1.4 * (max(Td_noise$ET.Eb[, ref], na.rm = TRUE) - 
        min(Td_noise$ET.Eb[, ref], na.rm = TRUE))
    for (rownum in rowlist) {
        Tdnew <- reviseb_witherror(rownum = rownum, Td_noise, 
            ref = ref)
        zero <- Tdnew$l.s.zero
        num <- c((rownum), rownum)
        if (AVE) {
            limits = c(zero - sdf, zero + sdf)
            blimits = c(-sdf, +sdf)
        }
        else {
            limits = c(zero - sdf, zero)
            blimits = c(-sdf, 0.3 * sdf)
        }
        plotsome(Tdnew, images = FALSE, indiv = TRUE, num = num, 
            transpose = TRUE, errb = TRUE, stderror = TRUE, of = TRUE, 
            limits = limits, genname = paste0("Row value=", floor(rownum), 
                " zero= ", round(zero, digits = 1), "\n"), row_unit = "", 
            col_unit = paste0("Data Column)"), z_unit = z_unit)
        num <- c(ref, ref)
        plotsquares(Tdnew, num = num, images = FALSE, indiv = TRUE, 
            of = FALSE, errb = TRUE, stderror = TRUE, slimits = c(0, 
                1.2), blimits = blimits, main = paste0("Row value=", 
                floor(rownum), "\nzero= ", round(zero, digits = 1)), 
            psf = FALSE, row_unit = "", col_unit = paste0("Column (Row value= ", 
                floor(rownum), " zero= ", round(zero, digits = 1)), 
            z_unit = z_unit)
        num <- c((rownum), rownum)
        plotsome(Tdnew, images = FALSE, indiv = TRUE, num = num, 
            transpose = TRUE, errb = TRUE, of = TRUE, limits = limits, 
            genname = paste0("Row value=", floor(rownum), " zero= ", 
                round(zero, digits = 1), "\n"), row_unit = "", 
            col_unit = paste0("Data Column)"), z_unit = z_unit)
        num <- c(ref, ref)
        plotsquares(Tdnew, num = num, images = FALSE, indiv = TRUE, 
            of = FALSE, errb = TRUE, slimits = c(0, 1.2), blimits = blimits, 
            main = paste0("Row value=", floor(rownum), "\nzero= ", 
                round(zero, digits = 1)), psf = FALSE, row_unit = "", 
            col_unit = paste0("Column (Row value= ", floor(rownum), 
                " zero= ", round(zero, digits = 1)), z_unit = z_unit)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
