\name{smoothSC}
\alias{smoothSC}
\title{Smoothing single-case data}
\description{
The \code{smoothSC} function provides procedures to smooth single-case data (i.e., to eliminate noise). A moving average function (mean- or median-based) replaces each data point by the average of the surrounding data points step-by-step. With a local regression function, each data point is regressed by its surrounding data points. 
}

\usage{
smoothSC(data, FUN = "movingMedian", intensity = NULL)
}

\arguments{
  \item{data}{A single-case data frame or a list of single-case data frames. See \code{\link{makeSCDF}} to learn about this format.}

  \item{FUN}{Function determining the smoothed scores. Default \code{FUN = "movingMedian"} is a moving Median function. Further possible values are: \code{"movingMean"} and a non-parametric \code{"localRegression"}.}
  \item{intensity}{For \code{FUN = "movingMedian"} and \code{"movingMean"} it is the lag used for computing the average. Default is \code{intensity = 1}. In case of \code{FUN = "localRegression"} it is the proportion of surrounding data influencing each data point, which is \code{intensity = 0.2} by default.}
}

\value{Returns a data frame (for each single-case) with smoothed data points. See \code{\link{makeSCDF}} to learn about the format of these data frames.}

\author{Juergen Wilbert}

\examples{
## Use the three different smoothing functions and compare the results
berta_mmd <- smoothSC(Huber2014$Berta)
berta_mmn <- smoothSC(Huber2014$Berta, FUN = "movingMean")
berta_lre <- smoothSC(Huber2014$Berta, FUN = "localRegression")
plotSC(list("Original" = Huber2014$Berta,"Moving Median" = berta_mmd[[1]],
    "Moving Mean" = berta_mmn[[1]],"Local Regression" = berta_lre[[1]]))
}

\keyword{manip}