\name{rSC}
\alias{rSC}
\title{Single-case data generator}
\description{
The \code{rSC} function generates random (multiple) single-case data frames for test and demonstration purposes.
}

\usage{
rSC(n = 1, MT = 20, B.start = 6, m = 50, s = 10, prob = 0.5, d.trend = 0, d.level = 0, 
    d.slope = 0, rtt = 0.8, concise = TRUE, cases = 1, round = NA, extreme.p = 0, 
    extreme.d = c(-4, -3), missing.p = 0, distribution = "normal", 
    start.values.fixed = FALSE, random.names = FALSE, output.long = FALSE)
}

\arguments{
  \item{n}{Number of cases to be created (Default is \code{n = 1}).}
  \item{MT}{Number of measurements (in each study). Default is \code{MT = 20}.}
  \item{B.start}{Phase B starting point. The default setting \code{B.start = 6} would assign the first five scores (of each case) to phase A, and all following scores to phase B. To assign different starting points for a set of multiple single-cases, use a vector of starting values (e.g. \code{B.start = c(6, 7, 8)}). If the number of cases exceeds the length of the vector, values will be repeated.}
  \item{m}{Mean of the sample distribution the scores are drawn from. Default is \code{m = 50}. To assign different means to several single-cases, use a vector of values (e.g. \code{m = c(50, 42, 56)}). If the number of cases exceeds the length of the vector, values are repeated.}
  \item{s}{Standard deviation of the sample distribution the scores are drawn from. Set to \code{s = 10} by default. To assign different variances to several single-cases, use a vector of values (e.g. \code{s = c(5, 10, 15)}). If the number of cases exceeds the length of the vector, values are repeated.}
  \item{prob}{If \code{distribution} (see below) is set \code{"binomial"}, \code{prob} passes the probability of occurrence.}
  \item{d.trend}{Defines the effect size \emph{d} of a trend per MT added across the whole data-set. To assign different trends to several single-cases, use a vector of values (e.g. \code{d.trend = c(.1, .3, .5)}). If the number of cases exceeds the length of the vector, values are repeated. While using a binomial or poisson distribution, \code{d.trend} indicates an increase in points / counts per MT.}
  \item{d.level}{Defines the level increase (effect size \emph{d}) at the beginning of phase B. To assign different level effects to several single-cases, use a vector of values (e.g. \code{d.level = c(.2, .4, .6)}). If the number of cases exceeds the length of the vector, values are repeated. While using a binomial or poisson distribution, \code{d.level} indicates an increase in points / counts per MT.}
  \item{d.slope}{Defines the increase in scores - starting with phase B - expressed as effect size \emph{d} per MT. \code{d.slope = .1} generates an incremental increase of 0.1 standard deviations per MT for all phase B measurements. To assign different slope effects to several single-cases, use a vector of values (e.g. \code{d.slope = c(.1, .2, .3)}). If the number of cases exceeds the length of the vector, values are repeated. While using a binomial or poisson distribution, \code{d.slope} indicates an increase in points / counts per MT.}
  \item{rtt}{Reliability of the underlying simulated measurements. Set \code{rtt = .8} by default. To assign different reliabilities to several single-cases, use a vector of values (e.g. \code{rtt = c(.6, .7, .8)}). If the number of cases exceeds the length of the vector, values are repeated. \code{rtt} has no effect when you're using binomial or poisson distributed scores.}
  \item{concise}{\code{TRUE} by default. If set \code{concise = FALSE}, input parameters (see above) for the sampling distribution are printed to the output.}
  \item{cases}{Number of cases per study. \code{rSC(n = 10, cases = 3, B.start = c(7,9,11))} creates 10 multiple-baseline designs, each with three cases (with phase B starting points at MT 7, 9, and 11).}
  \item{round}{Rounds the scores to the defined decimal. To round to the second decimal, set \code{round = 2}.}
  \item{extreme.p}{Probability of extreme values. \code{extreme.p = .05} gives a five percent probability of an extreme value. A vector of values assigns different probabilities to multiple cases. If the number of cases exceeds the length of the vector, values are repeated.}
  \item{extreme.d}{Range for extreme values, expressed as effect size \emph{d}. \code{extreme.d = c(-7,-6)} uses extreme values within a range of -7 and -6 standard deviations. In case of a binomial or poisson distribution, \code{extreme.d} indicates points / counts. Caution: the first value must be smaller than the second, otherwise the procedure will fail.}
  \item{missing.p}{Portion of missing values. \code{missing.p = 0.1} creates 10\% of all values as missing). A vector of values assigns different probabilities to multiple cases. If the number of cases exceeds the length of the vector, values are repeated.}
  \item{distribution}{Distribution of the scores. Default is \code{distribution = "normal"}. Possible values are \code{"normal"}, \code{"binomial"}, and \code{"poisson"}. If set to \code{"normal"}, the sample of scores will be normally distributed with the parameters \code{m} and \code{s} as mean and standard deviation of the sample, including a measurement error defined by \code{rtt}. If set to \code{"binomial"}, data are drawn from a binomial distribution with the expectation value \code{m}. This setting is useful for generating criterial data like correct answers in a test. If set to \code{"poisson"}, data are drawn from a poisson distribution, which is very common for count-data like behavioral observations. There's no measurement error is included. \code{m} defines the expectation value of the poisson distribution, lambda.}
  \item{start.values.fixed}{If set \code{TRUE}, the true value at the first measurement of each case is set to \code{m}. The default setting is \code{start.value.fixed = FALSE}, which results in a true value for each case drawn from a distribution (defined above: \code{distribution}) with the expected value \code{m} and standard deviation \code{s}.}
  \item{random.names}{Is \code{FALSE} by default. If set \code{random.names = TRUE} cases are assigned random first names. The names are drawn from the 2,000 most popular names for newborns in 2012 in the U.S. (1,000 male and 1,000 female names).}
  \item{output.long}{If set \code{TRUE}, the true value, implemented trend, level and slope effect, as well as error values are included for each MT. Default is \code{output.long = FALSE}.}
}

\value{
A single-case data frame or a list of single-case data frames. See \code{\link{makeSCDF}} to learn about this format.
}

\author{
Juergen Wibert
}

\examples{
## Create random single-case data and inspect it
dat <- rSC(n = 3, rtt = 0.75, round = 1, d.slope = 0.1, extreme.p = 0.1,
       missing.p = 0.1, random.names = TRUE)
describeSC(dat)
plotSC(dat)

## And now have a look at poisson-distributed data
dat <- rSC(n = 3, B.start = c(6,10,14), MT = c(12,20,22), m = 10,
       distribution = "poisson", d.level = -5, missing.p = 0.1)
pand(dat, correction = FALSE)
}

\keyword{datagen}