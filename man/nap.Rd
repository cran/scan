\name{nap}
\alias{nap}
\title{Nonoverlap of all Pairs}
\description{
The \code{nap} function calculates the nonoverlap of all pairs (NAP; Parker & Vannest, 2009).  NAP summarizes the overlap between all pairs of phase A and phase B data points.  If an increase of phase B scores is expected, a non-overlapping pair has a higher phase B data point.  The NAP equals \eqn{number of pairs showing no overlap / number of pairs}.  Because NAP can only take values between 50 and 100 percent, a rescaled and therefore more intuitive NAP (0-100\%) is also displayed.
}
\usage{
nap(data, decreasing = FALSE)
}
\arguments{
  \item{data}{
A single-case data frame. See \code{\link{makeSCDF}} to learn about this format.
}
  \item{decreasing}{
If you expect data to be lower in the B phase, set \code{decreasing = TRUE}. Default is \code{decreasing = FALSE}.
}
}
\value{
  \item{NAP}{Nonoverlap of all pairs.}
%  \item{Rescaled NAP}{NAP rescaled to 0-100\%.}
}
\references{
Parker, R. I., & Vannest, K. (2009). An improved effect size for single-case research: Nonoverlap of all pairs. \emph{Behavior Therapy, 40}, 357-367.
}

\author{Juergen Wilbert}

\seealso{\code{\link{overlapSC}}, \code{\link{describeSC}}, \code{\link{pand}}, \code{\link{pem}}, \code{\link{pet}}, \code{\link{pnd}}}

\examples{
## Calculate NAP for a study with  lower expected phase B scores (e.g. aggressive behavior)
gretchen <- makeSCDF(c(12,14,9,10,10,6,4,5,3,4), B.start = 5)
nap(gretchen, decreasing = TRUE)

## Request NAP for all cases fom the Grosche2011 data
lapply(Grosche2011, nap)
}

\concept{Overlap}