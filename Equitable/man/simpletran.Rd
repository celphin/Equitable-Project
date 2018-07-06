\name{simpletran}
\alias{simpletran}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
simple transform
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
simpletran(I, mat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{I}{
%%     ~~Describe \code{I} here~~
}
  \item{mat}{
%%     ~~Describe \code{mat} here~~
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
function (I, mat) 
{
    s <- mat$Es
    b <- mat$Eb
    p <- mat$Ep
    zw <- matrix(NA, nrow = nrow(I), ncol = ncol(I))
    zsd <- matrix(NA, nrow = nrow(I), ncol = ncol(I))
    EN <- matrix(NA, nrow = nrow(I), ncol = ncol(I))
    for (y in 1:ncol(I)) {
        for (t in 1:nrow(I)) {
            zw[t, y] <- weighted.mean(s[y, ] * I[t, ] + b[y, 
                ], (1 - p[y, ])^2/sum((1 - p[y, ])^2, na.rm = TRUE), 
                na.rm = TRUE)
            zsd[t, y] <- sqrt(sum((1 - p[y, ])^2/sum((1 - p[y, 
                ])^2, na.rm = TRUE) * (s[y, ] * I[t, ] + b[y, 
                ] - zw[t, y])^2, na.rm = TRUE))
            EN[t, y] <- length(which(complete.cases(s[y, ], b[y, 
                ])))
        }
    }
    cat("\n total of NA transformed data is ", sum(length(which(is.na(I)))))
    zw[which(is.na(zw))] <- I[which(is.na(zw))]
    Tx <- list(x = zw, xsd = zsd, EN = EN, Es = s, Eb = b, Ep = p)
    return(Tx)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
