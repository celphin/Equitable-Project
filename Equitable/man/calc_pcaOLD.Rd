\name{calc_pcaOLD}
\alias{calc_pcaOLD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
old calc princ comp anal
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
calc_pcaOLD(x = Td_noise$smat, main = " ", sectors.f = NULL)
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
function (x = Td_noise$smat, main = " ", sectors.f = NULL) 
{
    newdata <- as.data.frame(x)
    if (ncol(newdata) > 5) {
        fmm <- Mclust(na.omit(newdata))
        fmm
        table(fmm$classification)
        kcl <- kmeans(na.omit(newdata), 4, nstart = 25)
        print(table(fmm$classification, kcl$cluster))
        cat("\npartition into 4 clusters")
        aa <- sapply(1:4, function(j) {
            cat("\n Cluster ", j, "\n", names(kcl$cluster)[which(kcl$cluster == 
                j)], "\n")
        })
        colnames(newdata) <- paste0("p", 1:ncol(newdata))
        res.cov <- cov(newdata)
        eig <- eigen(res.cov)
        imagenan(eig$vectors, main = paste("Eigenvectors", main))
        rownames(eig$vectors) <- colnames(x)
        plot(eig$vectors[, 1], type = "b", pch = 15, main = "1st Principal Component Eigenvector")
        plot(eig$vectors[, 2], type = "b", pch = 15, main = "2nd Principal Component Eigenvector")
        plot(eig$vectors[, 3], type = "b", pch = 15, main = "3rd Principal Component Eigenvector")
        eigenvectors.t <- t(eig$vectors)
        newdata.t <- t(newdata)
        df.new <- eigenvectors.t \%*\% newdata.t
        df.new <- t(df.new)
        colnames(df.new) <- paste0("PC", 1:ncol(df.new))
        imagenan(df.new[, 1:3], main = "1st 3 Principal Components (Equiv to slopes)")
        plot(df.new[, 1], type = "b", pch = 15, main = "1st Principal Component DATA")
        plot(df.new[, 2], type = "b", pch = 15, main = "2nd Principal Component DATA")
        plot(df.new[, 3], type = "b", pch = 15, main = "3rd Principal Component DATA")
        xref <- which(min(Td$E.s[, 1], na.rm = TRUE) == Td$E.s[, 
            1])[1]
        tref <- which(min(Td$smat[1, ], na.rm = TRUE) == Td$smat[1, 
            ])[1]
        plot(Td$E.s[, xref], type = "b", pch = 15, main = "Slope along  Reference")
        plot(Td$E.s[, xref], eig$vectors[, 1])
        A <- matrix(NA, nrow = nrow(eigenvectors.t), ncol = nrow(eigenvectors.t))
        B <- matrix(NA, nrow = nrow(eigenvectors.t), ncol = nrow(eigenvectors.t))
        Itave <- colMeans(x, na.rm = TRUE)
        plot(Itave)
        B[, xref] <- Itave - Td$E.s[, xref] * Itave[xref]
        plot(Td$E.b[, xref])
        plot(Td$E.b[, tref])
        sapply(1:ncol(B), function(c) {
            plot(Td$E.b[, c], main = paste(c))
        })
        B <- sapply(1:ncol(B), function(c) {
            B[, c] <- B[, xref] - A[, c] * B[c, xref]
        })
        A[, xref] = eigenvectors.t[1, ]
        A <- sapply(1:ncol(A), function(c) {
            A[, c] <- A[, xref]/A[c, xref]
        })
        imagenan(A)
        imagenan(B)
        imagenan(Td$E.b)
        orig <- t(Td$smat)
        Tdata <- A \%*\% (orig - Td$l.s.zero)/ncol(A)
        imagenan(Tdata)
        sd(Tdata - t(Td$smat))
        ones <- matrix(rep(1, ncol(Td$ET.x) * nrow(Td$ET.x)), 
            nrow = ncol(Td$ET.x), ncol = nrow(Td$ET.x))
        bval <- Td$E.b \%*\% ones/ncol(Td$E.b)
        imagenan(bval)
        Tdata <- Tdata + bval + Td$l.s.zero
        Tdata <- A \%*\% orig/ncol(A) + B \%*\% ones/ncol(Td$E.b)
        imagenan(Tdata)
        sd(Tdata - t(Td$smat))
        imagenan(t(Td$smat))
        AA <- Td$E.s
        Tsimple <- AA \%*\% (orig - Td$l.s.zero)/ncol(Td$E.s)
        ones <- matrix(rep(1, ncol(Td$ET.x) * nrow(Td$ET.x)), 
            nrow = ncol(Td$ET.x), ncol = nrow(Td$ET.x))
        bval <- Td$E.b \%*\% ones/ncol(Td$E.b)
        imagenan(bval)
        Tsimple <- Tsimple + bval + Td$l.s.zero
        imagenan(Tsimple)
        sd(Tsimple - t(Td$smat))
        imagenan(Tsimple)
        sd(Tsimple - t(e1))
        plot(Tsimple, t(Td$smat), main = "Tsimple vs e1")
        imagenan(Td$Ave.ET.x)
        sd(Td$Ave.ET.x - (e1))
        plot(Td$Ave.ET.x, (e1), main = "TAve vs e1")
        plot(Td$smat[, "Row_Ave"])
        pca <- PCA(na.omit(newdata), graph = FALSE)
        print(fviz_eig(pca, addlabels = TRUE, main = paste("Scree Plot", 
            main)))
        print(fviz_pca_ind(pca, col.ind = "cos2", gradient.cols = c("#00AFBB", 
            "#E7B800", "#FC4E07"), repel = TRUE))
        print(fviz_pca_var(pca, col.var = "contrib", gradient.cols = c("#00AFBB", 
            "#E7B800", "#FC4E07"), repel = TRUE))
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
        biplot(fit, main = paste("PCA with ", numC, " Comp.", 
            main), var.axes = TRUE, lwd = 4)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
