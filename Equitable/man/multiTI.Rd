\name{multiTI}
\alias{multiTI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
multi Transforms with I
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
multiTI(Td_noise, numrun = 1, maxrun = 5, minme = 0.001, minstd = 0.015)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Td_noise}{
%%     ~~Describe \code{Td_noise} here~~
}
  \item{numrun}{
%%     ~~Describe \code{numrun} here~~
}
  \item{maxrun}{
%%     ~~Describe \code{maxrun} here~~
}
  \item{minme}{
%%     ~~Describe \code{minme} here~~
}
  \item{minstd}{
%%     ~~Describe \code{minstd} here~~
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
function (Td_noise, numrun = 1, maxrun = 5, minme = 0.001, minstd = 0.015) 
{
    if (numrun == 1) {
        imagenan(Td_noise$smat, main = "Original")
        imagenan(Td_noise$ET.x, main = "T[1]=T(I)")
    }
    cat("\n\nmultiTI: run ", numrun)
    if (numrun > maxrun) {
        cat("\n\n\nNo Convergence: ending at numrun=", numrun)
        if (!is.null(Td2)) {
            Td2$Ave.ET.x <- ET2$Ave.ET.x
            Td2$Ave.ET.xsd <- ET2$Ave.ET.xsd
            Td2$Ave.ET.Es <- ET2$Ave.ET.Es
            Td2$Ave.ET.Eb <- ET2$Ave.ET.Eb
            Td2$Ave.ET.Ep <- ET2$Ave.ET.Ep
        }
        else Td2 <- Td_noise
        return(Td_noise)
    }
    ET2 <- transave1(Td_noise$ET.x, Td_noise, x = 1:ncol(Td_noise$ET.x), 
        equita = TRUE, diagonal = TRUE)
    me <- abs(mean((ET2$Ave.ET.x - Td_noise$ET.x)/Td_noise$ET.x, 
        na.rm = TRUE))
    std <- sd((ET2$Ave.ET.x - Td_noise$ET.x)/Td_noise$ET.x, na.rm = TRUE)
    cat("\n\nmean of relative error", me)
    cat("\nsd of relative error", std)
    imagenan(ET2$Ave.ET.x, main = paste0("T(", numrun + 1, ")=T(T[", 
        numrun, "])"))
    Td2 <- Td_noise
    Td2$ET.x <- ET2$Ave.ET.x
    Td2$ET.xsd <- ET2$Ave.ET.xsd
    Td2$ET.Es <- ET2$Ave.ET.Es
    Td2$ET.Eb <- ET2$Ave.ET.Eb
    Td2$ET.Ep <- ET2$Ave.ET.Ep
    numrun <- numrun + 1
    if (me < minme && std < minstd) {
        cat("\n\n\nCONVERGENCE REACHED: T[n]=T[n-1] or T[n-1]=T(T[n-1])at numrun=", 
            numrun)
        Td2$Ave.ET.x <- ET2$Ave.ET.x
        Td2$Ave.ET.xsd <- ET2$Ave.ET.xsd
        Td2$Ave.ET.Es <- ET2$Ave.ET.Es
        Td2$Ave.ET.Eb <- ET2$Ave.ET.Eb
        Td2$Ave.ET.Ep <- ET2$Ave.ET.Ep
        return(Td2)
    }
    multiTI(Td_noise = Td2, numrun = numrun)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
