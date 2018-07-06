\name{parallel_Etest}
\alias{parallel_Etest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
test for parallel lines
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
parallel_Etest(Es, p, p_par, slope = 1, scritical = 0.1, pcritical = 1e-05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Es}{
%%     ~~Describe \code{Es} here~~
}
  \item{p}{
%%     ~~Describe \code{p} here~~
}
  \item{p_par}{
%%     ~~Describe \code{p_par} here~~
}
  \item{slope}{
%%     ~~Describe \code{slope} here~~
}
  \item{scritical}{
%%     ~~Describe \code{scritical} here~~
}
  \item{pcritical}{
%%     ~~Describe \code{pcritical} here~~
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
function (Es, p, p_par, slope = 1, scritical = 0.1, pcritical = 1e-05) 
{
    imagenan(p_par, zlim = c(0.01, 0.05), outside.above.color = "red", 
        main = "Enter Etest :Parallel=red")
    for (r in 1:nrow(Es)) {
        for (c in 1:ncol(Es)) {
            pv <- p[r, c]
            if (!is.na(pv) && !is.nan(pv) && !is.infinite(pv)) {
                if (pv < pcritical && !is.na(Es[r, c]) && !is.nan(Es[r, 
                  c])) {
                  if (abs((slope - Es[r, c])/slope) < scritical) 
                    p_par[r, c] = 1
                  else p_par[r, c] = 0
                }
            }
        }
    }
    imagenan(p_par, zlim = c(0.01, 0.05), outside.above.color = "red", 
        main = "Exit Etest :Parallel=red")
    cat("\n(At 95\% Confidence: Proportion of parallel slopes(NULL)", 
        (length(which(p_par > 0.05)))/length(p_par[which(!is.na(p_par))]))
    cat(" Non parallel ", (length(which(p_par <= 0.05)))/length(p_par[which(!is.na(p_par))]), 
        "\n")
    cat("    98\%: Proportion of parallel slopes ", (length(which(p_par > 
        0.02)))/length(p_par[which(!is.na(p_par))]))
    cat(" Non parallel ", (length(which(p_par <= 0.02)))/length(p_par[which(!is.na(p_par))]), 
        "\n\n")
    return(p_par)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
