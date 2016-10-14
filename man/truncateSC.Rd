\name{truncateSC}
\alias{truncateSC}
\title{Truncate single-case data}
\description{
This function truncates data points at the beginning and / or end of both phases.
}

\usage{
truncateSC(data, A = c(0,0), B = c(0,0))
}

\arguments{
  \item{data}{A single-case data frame or a list of single-case data frames. See \code{\link{makeSCDF}} to learn about this format.}
  \item{A}{A vector of two values defining the number of data points from phase A to be dropped (at the beginning and end). For lists of single-case data frames, the truncation is adapted to the length of the A phase for each single case.}
  \item{B}{A vector of two values defining the number of data points from phase B to be dropped (at the beginning and end). For lists of single-case data frames, the truncation is adapted to the length of the B phase for each single case.}
}

\value{A truncated data frame (for each single-case).}

\author{Juergen Wilbert}

\seealso{\code{\link{outlierSC}}, \code{\link{fillmissingSC}}, \code{\link{makeSCDF}}}

\examples{
# Truncate the first two data points of both phases and compare the two data sets
truHeart <- truncateSC(byHeart2011[1], A = c(2,0), B = c(2,0))
plotSC(c(original = byHeart2011[1], selected = truHeart))
}

\keyword{manip}