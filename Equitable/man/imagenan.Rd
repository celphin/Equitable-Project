% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Equitable_Transform.R
\name{imagenan}
\alias{imagenan}
\title{Plots False Colour Image}
\usage{
imagenan(x, yline = 3, yma = 5, col = topo.colors(255),
  outside.below.color = "black", outside.above.color = "white",
  na.color = "gray", ...)
}
\arguments{
\item{x}{2D data to display}

\item{yline}{Number of lines in (left side) to where image begins}

\item{yma}{Number of characters in (left side)}

\item{col}{colour table}

\item{outside.below.color}{Colour below threshold}

\item{outside.above.color}{Colour above threshold}

\item{na.color}{Colour of NAs (see \code{\link[graphics]{par}})}

\item{...}{other plot parameters eg zlim list with min and max range of color table data e.g. zlim=c(2,5)}
}
\value{
None
}
\description{
Displays a false colour image of two dimensional data set
}
\examples{
d<-eg4()
imagenan(d)
imagenan(d,zlim=c(2,8))
imagenan(d,zlim=c(3,8),outside.above.color='red',outside.below.color='tan')
d=data.frame(cbind(c(1:4),c(2,5,NA,NA) ))
imagenan(d)
}
