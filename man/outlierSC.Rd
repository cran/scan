\name{outlierSC}
\alias{outlierSC}
\title{Handling outliers in single-case data}
\description{
Identifies and drops outliers within a single-case data frame or a list of single-case data frames.
}

\usage{
outlierSC(data, criteria = c("SD", "2"))
}

\arguments{
  \item{data}{
A single-case data frame or a list of single-case data frames. See \code{\link{makeSCDF}} to learn about this format.}
  \item{criteria}{Specifies the criteria for outlier identification. Set \code{criteria = c("SD", 2)} to define two standard deviations as limit. This is also the default setting. To use the 99\% Confidence Interval use \code{criteria = c("CI", 0.99)}. Set \code{criteria = c("Cook", "4/n")} to define any data point with a Cook's Distance greater than 4/n as an outlier, based on the Piecewise Linear Regression Model.}
}

\value{
  \item{data}{A data frame (for each single-case) without outliers.}
  \item{dropped.n}{A list with the number of dropped data points for each single-case.}
  \item{dropped.mt}{A list with the measurement-times of dropped data points for each single-case (values are based on the \code{mt} variable of each single-case data frame).}
  \item{sd.matrix}{A list with a matrix for each case with values for the upper and lower boundaries based on the standard deviation.}
  \item{ci.matrix}{A list with a matrix for each single-case with values for the upper and lower boundaries based on the confidence interval.}
  \item{cook}{A list of Cook's Distances for each measurement of each single-case.}
  \item{criteria}{Criteria used for outlier analysis.}
  \item{N}{Number of single-cases.}
  \item{case.names}{Case identifier.}
}

\author{Juergen Wilbert}

\seealso{\code{\link{describeSC}}, \code{\link{fillmissingSC}}, \code{\link{plotSC}}}

\examples{
## Identify outliers using 1.5 standard deviations as criterion
susanne <- rSC(d.level = 1.0)
res <- outlierSC(susanne, criteria = c("SD", 1.5))
plotSC(susanne, marks = list(positions = res$dropped.mt))

## Identify outliers in the original data from Grosche (2011) using Cook's Distance
## greater than 4/n as criterion
res <- outlierSC(Grosche2011, criteria = c("Cook", "4/n"))
plotSC(Grosche2011, marks = list(positions = res$dropped.mt))
}

\keyword{manip}

\concept{Outliers}
\concept{Extreme Values}