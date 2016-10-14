\name{pet}
\alias{pet}
\title{Percent exceeding the trend}
\description{
The \code{pet} function provides the percentage of phase B data points exceeding the prediction based on the phase A trend. A binomial test against a 50/50 distribution is computed. Furthermore, the percentage of phase B data points exceeding the upper (or lower) 95 percent confidence interval of the predicted progress is computed.
}
\usage{
pet(data, ci = 0.95, decreasing = FALSE)
}
\arguments{
  \item{data}{
A single-case data frame or a list of single-case data frames. See \code{\link{makeSCDF}} to learn about this format.
}
  \item{decreasing}{
If you expect data to be lower in the B phase, set \code{decreasing = TRUE}. Default is \code{decreasing = FALSE}.
}
  \item{ci}{
Width of the confidence interval. Default is \code{ci = .95}.
}
}
\value{
  \item{PET}{Percent exceeding the trend.}
  \item{PET.ci}{Percent exceeding the upper / lower 95\%-CI boundary.}
  \item{p}{P value of Binomial Test.}
  \item{ci.percent}{Width of confidence interval in percent.}
  \item{se.factors}{Standard error.}
  \item{N}{Number of cases.}
  \item{decreasing}{Logical argument from function call (see \code{Arguments} above).}
  \item{case.names}{Assigned name of single-case.}
}
\author{Juergen Wilbert}

\seealso{\code{\link{overlapSC}}, \code{\link{describeSC}}, \code{\link{nap}}, \code{\link{pand}}, \code{\link{pem}}, \code{\link{pnd}}}

\examples{
## Calculate the PET and use a 99\%-CI for the additional calculation
dat <- rSC(5, d.slope = 0.2)
pet(dat, ci = .99)
}

\concept{Overlap}