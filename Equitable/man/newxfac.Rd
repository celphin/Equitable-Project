\name{newxfac}
\alias{newxfac}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
make new factors
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
newxfac(dissimilarity = Td6_9$ET.x, sname, dendfactor = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dissimilarity}{
%%     ~~Describe \code{dissimilarity} here~~
}
  \item{sname}{
%%     ~~Describe \code{sname} here~~
}
  \item{dendfactor}{
%%     ~~Describe \code{dendfactor} here~~
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
function (dissimilarity = Td6_9$ET.x, sname, dendfactor = NULL) 
{
    xfac <- allfactors(sname)
    xf <- NULL
    for (jp in 1:ncol(dissimilarity)) xf <- c(xf, which(colnames(dissimilarity)[jp] == 
        rownames(xfac)))
    xf
    xfac <- xfac[xf, ]
    rownames(xfac)
    k <- which(colnames(dissimilarity) == "Row_Ave")
    if (length(k) > 0) {
        xfac1 <- xfac
        for (j in 1:length(k)) {
            if (k[j] <= nrow(xfac)) {
                xfac1 <- rbind(xfac[1:(k[j] - 1), ], rep(NA, 
                  ncol(xfac)), xfac[k[j]:(nrow(xfac)), ])
            }
            else {
                xfac1 <- rbind(xfac[1:(k[j] - 1), ], rep(NA, 
                  ncol(xfac)))
            }
            rnn <- paste0("Row_Ave", j)
            rownames(xfac1)[k[j]] <- rnn
            alist <- c("Id", "Plot", "Year", "Plant.Plot.ID", 
                "Plant.Field.ID", "Snow", "Average.Temp", "June.Temp", 
                "July.Temp", "August.Temp")
            for (jj in alist) xfac1[rnn, jj] <- round(mean(xfac[, 
                jj], na.rm = TRUE), digits = 1)
            colnames(dissimilarity)[k[j]] <- paste0("Row_Ave", 
                j)
            xfac <- xfac1
        }
    }
    if (!is.null(dendfactor)) {
        xfac <- cbind(xfac, dendfactor)
        colnames(xfac)[ncol(xfac)] <- "Dendo_branches"
    }
    cat("\n\n\nlength of xfac", nrow(xfac), "number of columns dissimilarity ", 
        ncol(dissimilarity))
    return(xfac)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
