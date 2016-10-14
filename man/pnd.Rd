\name{pnd}
\alias{pnd}
\title{Percentage of non-overlapping data}
\description{
This function returns the percentage of non-overlapping data.  Due to its error-proneness the PND should not be used, but \code{\link{nap}} or \code{\link{pand}} instead (see Parker & Vannest, 2009).
}
\usage{
pnd(data, decreasing = FALSE)
}

\arguments{
  \item{data}{A single-case data frame or a list of single-case data frames. See \code{\link{makeSCDF}} to learn about this format.}
  \item{decreasing}{If you expect data to be lower in the B phase, set \code{decreasing = TRUE}. Default is \code{decreasing = FALSE}.}
}

\value{
  \item{PND}{Percentage of non-overlapping data.}
}

\author{Juergen Wilbert}

\seealso{\code{\link{overlapSC}}, \code{\link{describeSC}}, \code{\link{nap}}, \code{\link{pand}}, \code{\link{pem}}, \code{\link{pet}}}

\examples{
## Calculate the PND for multiple single-case data
pnd(GruenkeWilbert2014)
}

\concept{Overlap}