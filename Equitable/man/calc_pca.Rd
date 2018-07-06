\name{calc_pca}
\alias{calc_pca}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
calculate principle components of data
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
calc_pca(x = Td_noise$smat, main = " ", sectors.f = NULL, f_4 = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
}
  \item{sectors.f}{
%%     ~~Describe \code{sectors.f} here~~
}
  \item{f_4}{
%%     ~~Describe \code{f_4} here~~
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
function (x = Td_noise$smat, main = " ", sectors.f = NULL, f_4 = TRUE) 
{
    newdata <- as.data.frame(x)
    if (ncol(newdata) > 5) {
        fmm <- Mclust(na.omit(newdata))
        fmm
        table(fmm$classification)
        kcl <- kmeans(na.omit(newdata), 4, nstart = 25)
        print(table(fmm$classification, kcl$cluster))
        cat("\n", main, " partition into 4 clusters")
        aa <- sapply(1:4, function(j) {
            cat("\n Cluster ", j, "\n", names(kcl$cluster)[which(kcl$cluster == 
                j)], "\n")
        })
        colnames(newdata) <- paste0("p", 1:ncol(newdata))
        pca <- PCA(na.omit(newdata), graph = TRUE)
        print(fviz_eig(pca, addlabels = TRUE, main = paste("Scree Plot", 
            main)))
        plot(pca, choix = c("ind"), label = "none", col.ind = fmm$classification, 
            cex = 3)
        legend("bottom", cex = 1, legend = paste("General Clusters for", 
            main))
        plot(pca, choix = c("ind"), label = "none", col.ind = kcl$cluster, 
            cex = 3)
        clu <- factor(kcl$cluster)
        summary(clu)
        legend("topleft", cex = 1, legend = paste("Cluster", 
            levels(clu)), fill = levels(clu))
        legend("bottom", cex = 1, legend = paste("4 Clusters for", 
            main))
        if (!is.null(sectors.f)) {
            cat("\nStart Sectors colouring for PCA\n")
            summary(sectors.f)
            cols = as.double(sectors.f)
            names(cols) <- paste0("y_", names(sectors.f))
            plot(pca, choix = c("ind"), label = "none", col.ind = cols[names(kcl$cluster)], 
                cex = 3)
            legend("topleft", cex = 1, legend = paste("Region", 
                levels(sectors.f)), fill = 1:nlevels(sectors.f), 
                merge = F, bty = "n")
            legend("bottomright", cex = 1, legend = paste(" Regions for", 
                main))
        }
        numC <- 3
        fit <- pca(na.omit(newdata), scale = "uv", nPcs = numC, 
            center = TRUE)
        plotPcs(fit, pcs = 1:nP(fit), type = c("scores", "loadings"), 
            sl = NULL, hotelling = 0.5, col = kcl$cluster, pch = 15, 
            cex = 1.5)
        cat("\n", main, "\n")
        print(summary(fit))
    }
    if (f_4) 
        return(kcl$cluster)
    else return(fmm$classification)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
