\name{makeSCDF}
\alias{makeSCDF}
\title{Create a single case data frame for further analyses}
\description{
The \code{makeSCDF} function creates the basic data structure for one or multiple single-cases.  This basic data structure is required in all \code{scan} functions.
}

\usage{
makeSCDF(data, B.start = NULL, MT = NULL)
}

\arguments{
  \item{data}{
A vector containing measurement scores.
}
  \item{B.start}{
The first measurement of phase B.
}
  \item{MT}{
A vector defining measurement times. Default is \code{MT = (1,2,3,...,n)}.
}
}

\value{
Returns a data frame formatted as a single-case data frame suitable for all functions of the \code{scan} package. Multiple data sets (e.g. from Multiple Baseline Designs) can be listed. 
}

\author{Juergen Wilbert}

\seealso{\code{\link{longSCDF}}, \code{\link{makesingleSC}}, \code{\link{writeSC}}}

\examples{
## Scores on a letter naming task were collected on eleven days in a row. The intervention
## started after the fifth measurement, so the first B phase measurement was 6 (B.start = 6).
klaas <- makeSCDF(c(5, 7, 8, 5, 7, 12, 16, 18, 15, 14, 19), B.start = 6)
plotSC(klaas)

## Unfortunately in a similar SCDR there were no data collected on days 3 and 9. Use NA to
## pass them to the package.
emmi <- makeSCDF(c(5, 7, NA, 5, 7, 12, 16, 18, NA, 14, 19), B.start = 6)
describeSC(emmi)

## In a MBD over three persons, data were again collected eleven days in a row. Intervention
## starting points differ between subjects as they were randomly assigned. The three SCDFs
## are then combined in a list for further conjoined analyses.
charlotte <- makeSCDF(c(5, 7, 10, 5, 12, 7, 10, 18, 15, 14, 19), 6)
theresa <- makeSCDF(c(3, 4, 3, 5, 7, 4, 7, 9, 8, 10, 12),5)
antonia <- makeSCDF(c(9, 8, 8, 7, 5, 7, 6, 14, 15, 12, 16), 7)
mbd <- list(charlotte, theresa, antonia)
names(mbd) <- c("Charlotte", "Theresa", "Antonia")
overlapSC(mbd)

## In a classroom-based intervention it was not possible to measure outcomes every day, but
## only on schooldays. The sequence of measurements is passed to the package by using a
## vector of measurement times.
frida <- makeSCDF(c(3, 2, 4, 2, 2, 3, 5, 6, 8, 10, 8, 12, 14, 13, 12), B.start = 9,
    MT = c(1, 2, 3, 4, 5, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18))
plotSC(frida)
describeSC(frida)
}