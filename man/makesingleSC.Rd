\name{makesingleSC}
\alias{makesingleSC}
\title{Aggregate multiple single-cases into one case}
\description{
The makesingleSC function combines multiple single-case data frames into one single-case data frame.
}
\usage{
makesingleSC(data, scale = FALSE, type = "add")
}
\arguments{
  \item{data}{
A vector with measurements, a data frame or a list of data frames.
}
  \item{scale}{Unused}
  \item{type}{By default values with the same measurement are added. If type is set to "mean" or "median", values of the same measurement are replaced with their mean or median. Default is "add".
}
}
\details{
The algorithm works the following way:
\enumerate{
  \item{All values of each single-case are centred with respect to each case's phase A mean.}
  \item{The phase A values of all single-cases are combined in ascending order of their measurement times.}
  \item{The phase B values of all single-cases are combined in ascending order of their measurement times.}
  \item{Phase B values are appended to phase A values. The measurement times of phase B are shifted to start with the next MT after the end of phase A.}
}
}
\author{Juergen Wilbert}

\seealso{\code{\link{makeSCDF}}, \code{\link{longSCDF}}, \code{\link{writeSC}}}

\examples{
## Aggregate three random single-case data sets into one
study1 <- rSC(3, d.level = 0.8, d.slope = 0.1, round = 0)
new.sc <- makesingleSC(study1)
plotSC(new.sc, type = "p", lines = "trend")
}

\keyword{manip}