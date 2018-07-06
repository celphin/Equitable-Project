\name{transformE}
\alias{transformE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
calculates transform
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
transformE(d, Ave = TRUE, cAve = FALSE, Zero = FALSE, zero = NULL, diagonal = TRUE, imageplot = FALSE, old = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d}{
%%     ~~Describe \code{d} here~~
}
  \item{Ave}{
%%     ~~Describe \code{Ave} here~~
}
  \item{cAve}{
%%     ~~Describe \code{cAve} here~~
}
  \item{Zero}{
%%     ~~Describe \code{Zero} here~~
}
  \item{zero}{
%%     ~~Describe \code{zero} here~~
}
  \item{diagonal}{
%%     ~~Describe \code{diagonal} here~~
}
  \item{imageplot}{
%%     ~~Describe \code{imageplot} here~~
}
  \item{old}{
%%     ~~Describe \code{old} here~~
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
function (d, Ave = TRUE, cAve = FALSE, Zero = FALSE, zero = NULL, 
    diagonal = TRUE, imageplot = FALSE, old = NULL) 
{
    d <- as.matrix(d, nrow = nrow(d), ncol = ncol(d))
    if (nrow(d) < 4 || ncol(d) < 3) 
        return()
    if (sd(d, na.rm = TRUE) < 1e-08) 
        return()
    if (Zero) {
        if (is.null(zero)) 
            zero <- mean(d, na.rm = TRUE)
        d <- d - zero
        cat("\n zero set to ", zero)
    }
    else zero = 0
    if (Ave) 
        smat <- addave(d, cAve = cAve)
    else smat <- d
    cat("\n DATA SET HAS ", nrow(d), ncol(d), " Rows and Columns\n")
    if (imageplot) 
        imagenan(smat, main = paste0("Input Data Set"))
    m <- corre(smat)
    m$zero = zero
    Tx <- transf(smat, m, equita = FALSE, diagonal = diagonal)
    if (imageplot) 
        imagenan(Tx$x, main = paste0("Least Squared Transform"))
    E <- equitable(m)
    m$p_par <- parallel_Etest(Es = E$s, p = m$pslope, p_par = m$p_par)
    Em <- m
    Em$E.s <- E$s
    if (!is.null(old)) {
        Em$E.s[is.na(old$ET.Es)] <- NA
    }
    Em$E.sd1 <- E$sd1
    Em$E.snode <- E$snode
    Eb <- equitableb(Em)
    Em$E.b <- Eb$b
    ET <- transf(smat, Em, equita = TRUE, diagonal = diagonal)
    if (imageplot) 
        imagenan(ET$x, main = paste0("Equitable Transform"))
    names(m) <- paste0("l.s.", names(m))
    names(Tx) <- paste0("l.s.", names(Tx))
    names(E) <- paste0("E.", names(E))
    names(Eb) <- paste0("E.", names(Eb))
    names(ET) <- paste0("ET.", names(ET))
    cat("\nUsing average as Reference to make transform")
    ETave <- transave(smat, Em, equita = TRUE)
    smat <- list(smat = smat)
    TransE <- c(smat, m, Tx, E, Eb, ET, ETave)
    if (Zero) {
        TransE$smat <- TransE$smat + zero
        TransE$ET.x <- TransE$ET.x + zero
        TransE$l.s.x <- TransE$l.s.x + zero
        TransE$Ave.ET.x <- TransE$Ave.ET.x + zero
    }
    return(TransE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
