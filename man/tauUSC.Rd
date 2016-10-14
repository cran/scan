\name{tauUSC}
\alias{tauUSC}
\title{Tau-U for single-case data}
\description{
This function calculates indices of the Tau-U family as proposed by Parker et al. (2011).
}
\usage{
tauUSC(data, ties.method = "omit")
}

\arguments{
  \item{data}{
A single-case data frame.
}
  \item{ties.method}{
Defines how to handle ties. \code{"omit"} excludes all ties from the calculation. \code{"positive"} counts all ties as positive comparisons, while \code{"negative"} counts them as negative comparisons.
}
}

\value{
  \item{table}{A data frame containing statistics from the Tau-U family, including: Pairs, positive and negative comparisons, S, and Tau}
  \item{matrix}{The matrix of comparisons used for calculating the statistics.}
  \item{tau_u}{Tau-U value.}
}
\references{
Parker, R. I., Vannest, K. J., Davis, J. L., & Sauber, S. B. (2011). Combining Nonoverlap and Trend for Single-Case Research: Tau-U. \emph{Behavior Therapy, 42}, 284-299.  
}
\author{
Juergen Wilbert
}
\examples{
## Calculate tau-U for the example from Parker et al. (2011)
bob <- makeSCDF(c(2, 3, 5, 3, 4, 5, 5, 7, 6), B.start = 5)
tauUSC(bob)

## Calculate tau-U with ties counted as positive
tauUSC(Grosche2011$Eva, ties.method = "positive")

## Request tau-U for all single-cases fom the Grosche2011 data
lapply(Grosche2011, tauUSC)
}
