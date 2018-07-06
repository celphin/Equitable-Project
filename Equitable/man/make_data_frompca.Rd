\name{make_data_frompca}
\alias{make_data_frompca}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
make data from principle component
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
make_data_frompca(orig, main = " ")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{orig}{
%%     ~~Describe \code{orig} here~~
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
function (orig, main = " ") 
{
    orig <- as.data.frame(orig)
    if (ncol(orig) > 3) {
        res.cov <- cov(orig)
        eig <- eigen(res.cov)
        imagenan(eig$vectors, main = paste("Eigenvectors", main))
        rownames(eig$vectors) <- colnames(orig)
        plot(eig$vectors[, 1], type = "b", pch = 15, main = "1st Principal Component Eigenvector")
        plot(eig$vectors[, 2], type = "b", pch = 15, main = "2nd Principal Component Eigenvector")
        plot(eig$vectors[, 3], type = "b", pch = 15, main = "3rd Principal Component Eigenvector")
        newdata.t <- t(orig)
        eigenvectors.t <- t(eig$vectors)
        df.new <- eigenvectors.t \%*\% newdata.t
        df.new <- t(df.new)
        colnames(df.new) <- paste0("PC", 1:ncol(df.new))
        imagenan(df.new[, 1:3], main = "1st 3 Principal Components (Equiv to slopes)")
        plot(df.new[, 1], type = "b", pch = 15, main = "1st Principal Component DATA:g1(t)*sigma(f1)^2")
        plot(df.new[, 2], type = "b", pch = 15, main = "2nd Principal Component DATA:g2(t)*sigma(f2)^2")
        plot(df.new[, 3], type = "b", pch = 15, main = "3rd Principal Component DATA:g3(t)*sigma(f3)^2")
        eigenvect <- eigenvectors.t[1, ]
        Tdata1 <- make_datafrom_eigen(eigenvect = eigenvect, 
            orig = orig, xref = 1, main = "1st Eigenvector")
        Tx1 <- Tdata1$Tx
        eigenvect <- eigenvectors.t[2, ]
        orig1 <- orig - Tx1
        Tdata2 <- make_datafrom_eigen(eigenvect = eigenvect, 
            orig = orig1, xref = 2, sgn = (1), main = "2nd Eigenvector")
        Tx2 <- Tdata2$Tx
        eigenvect <- eigenvectors.t[3, ]
        orig2 <- orig1 - Tx2
        Tdata3 <- make_datafrom_eigen(eigenvect = eigenvect, 
            orig = orig2, xref = 3, sgn = (1), main = "3rd Eigenvector")
        Tx3 <- Tdata3$Tx
        Teigen <- list(e1 = Tx1, e2 = Tx2, e3 = Tx3)
    }
    return(Teigen)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
