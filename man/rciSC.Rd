\name{rciSC}
\alias{rciSC}
\alias{rCi}
\title{Reliable change index}
\description{
\bold{CAUTION! This function is still under development and not ready for use!} The \code{rciSC} function computes three indices of reliable change (Wise, 2004) and corresponding descriptive statistics.
}
\usage{
rciSC(data, rel = 0.8, ci = 0.95, graph = FALSE)
}

\arguments{
  \item{data}{
A single-case data frame. See \code{\link{makeSCDF}} to learn about this format.
}
  \item{rel}{
Reliability of the measure, used to compute the standard error. Default is \code{rel = 0.8}.
}
  \item{ci}{
Width of confidence interval as a decimal. Default is \code{ci = 0.95} applying a 95\%-confidence interval.
}
  \item{graph}{
If set \code{TRUE}, a box plot of phase A and B scores is displayed. \code{graph = FALSE} by default.}
}

\value{
  \item{RCI}{A list of three RCI calculations (Jacobson et al., Christenden et al., Hageman et al.).}
  \item{stand.dif}{Standardized difference between mean phase A and B scores.}
  \item{conf}{A matrix containing the lower and upper confidence interval boundaries of phases A and B.}
  \item{conf.percent}{Numeric argument from function call (see \code{ci} in \code{Arguments} section).}
  \item{reliability}{Numeric argument from function call (see \code{Arguments} above).}
  \item{descriptives}{A matrix containing descriptive statistics for phases A and B: n, mean, SD, SE.}
  \item{N}{Number of cases.}
  \item{A}{A vector of phase A scores.}
  \item{B}{A vector of phase B scores.}
}
\author{Juergen Wilbert}

\references{
Christensen, L., & Mendoza, J. L. (1986). A method of assessing change in a single subject: An alteration of the RC index. \emph{Behavior Therapy, 17}, 305-308.

Hageman, W. J. J., & Arrindell, W. A. (1993). A further refinement of the reliable change (RC) index by improving the pre-post difference score: Introducing RCID. \emph{Behaviour Research and Therapy, 31}, 693-700.

Jacobson, N. S., & Truax, P. (1991). Clinical Significance: A statistical approach to defining meaningful change in psychotherapy research. \emph{Journal of Consulting and Clinical Psychology, 59}, 12-19.

Wise, E. A. (2004). Methods for analyzing psychotherapy outcomes: A review of clinical significance, reliable change, and recommendations for future directions. \emph{Journal of Personality Assessment, 82}, 50 - 59.
}

\examples{
## Report the RCIs of the first case from the byHeart data and include a graph
rciSC(byHeart2011[1], graph = TRUE)
}