\name{trendSC}
\alias{trendSC}
\title{Trend analysis for single-cases data}
\description{
The \code{trendSC} function provides an overview of linear trends in single-case data.  By default, it gives you the intercept and slope of a linear and a squared regression of measurement-time on scores.  Models are computed separately for the A phase, the B-phase, and the whole data.  For a more advanced application, you can add regression models using the R specific formula class.
}

\usage{
trendSC(data, B.offset = -1, model = NA)
}

\arguments{
  \item{data}{
A single-case data frame. See \code{\link{makeSCDF}} to learn about this format.
}

  \item{B.offset}{
An offset for the first phase B measurement-time (MT). If set \code{B.offset = 0}, the first phase B measurement is handled as MT 1. Default is \code{B.offset = -1}, making the first value of phase B MT = 0.
}
  \item{model}{
A string or a list of (named) strings each depicting one regression model. This is a formula expression of the standard R class. The parameters of the model are \code{values}, \code{mt} and \code{phase}. 
}
}

\value{
  \item{trend}{A matrix containing the results (Intercept, B and beta) of separate regression models for phase A, phase B, and the whole data.}
  \item{B.offset}{Numeric argument from function call (see \code{Arguments} section).}
}

\author{Juergen Wilbert}

\seealso{\code{\link{describeSC}}, \code{\link{overlapSC}}, \code{\link{plm}}, \code{\link{hplm}}}

\examples{
## Compute the linear and squared regression for a random single-case
matthea <- rSC(d.slope = 0.5)
trendSC(matthea)

## Besides the linear and squared regression models compute two custom models:
## a) a cubic model, and b) the values predicted by the natural logarithm of the
## measurement time.
ben <- rSC(d.slope = 0.3)
trendSC(ben, B.offset = 0, model = c("Cubic" = "values ~ I(mt^3)", "Log Time" = "values ~ log(mt)"))
}

\concept{Trend}