\name{autocorrSC}
\alias{autocorrSC}
\title{Autocorrelation for single-case data}
\description{
The autocorrSC function calculates autocorrelations within phase A, phase B and the complete single-case data set (AB).
}

\usage{
autocorrSC(data, lag.max = 3)
}

\arguments{
  \item{data}{A single-case data frame or a list of single-case data frames. See \code{\link{makeSCDF}} to learn about this format.}
  \item{lag.max}{The lag up to which autocorrelations will be computed. Default is \code{lag.max = 3}.}
}
\value{
  \item{autocorr}{A data frame containing separate autocorrelations of phase A, phase B and the complete data (for each single-case). If \code{lag.max} exceeds the length of a phase minus one, NA is returned for this cell.}
}

\author{Juergen Wilbert}

\seealso{\code{\link{trendSC}}, \code{\link{plm}}, \code{\link{hplm}}}

\examples{
## Compute autocorrelations for a list of four single-cases with max.lag = 2
autocorrSC(Huber2014, lag.max = 2)
}

\concept{Autocorrelation}
\concept{Serial Correlation}