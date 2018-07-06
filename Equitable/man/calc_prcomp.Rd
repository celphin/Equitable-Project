\name{calc_prcomp}
\alias{calc_prcomp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
different pr comp anal
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
calc_prcomp(x = Td$smat, main = " ")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
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
function (x = Td$smat, main = " ") 
{
    op = par(mfrow = c(1, 1), mar = c(7, 4, 4.5, 4))
    imagenan(x, main = main)
    newdata <- as.data.frame(x)
    colnames(newdata) <- paste0("p", 1:ncol(newdata))
    fit <- prcomp(formula = ~., data = newdata, scale = TRUE, 
        na.action = na.exclude)
    print(summary(fit))
    print(loadings(fit))
    plot(fit, ylim = c(0, fit$sdev[1]^2), main = paste("PCA"))
    biplot(fit, main = paste("PCA", main), var.axes = TRUE, lwd = 4)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
