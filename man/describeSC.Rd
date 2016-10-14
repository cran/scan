\name{describeSC}
\alias{describeSC}
\title{Descriptive statistics for single-case data}
\description{
The \code{describeSC} function provides common descriptive statistics for single-case data.
}

\usage{
describeSC(data, decreasing = FALSE)
}

\arguments{
  \item{data}{A single-case data frame or a list of single-case data frames. See \code{\link{makeSCDF}} to learn about this format.}
  \item{decreasing}{If you expect data to be lower in phase B, set \code{decreasing = TRUE}. At the moment describeSC only reports indices not sensible to decreasing phase B scores, so this does not change the output.}
}

\value{A data frame (or a list of data frames) of descriptive statistics (for each single-case), i.a.: number of observations, number of missing values, measures of central tendency and variation, trend, SMD.}

\author{Juergen Wilbert}

\seealso{\code{\link{overlapSC}}, \code{\link{plotSC}}, \code{\link{writeSC}}}

\examples{
## Descriptive statistics for one single-case
describeSC(Waddell2011)

## Descriptive statistics for a list of three single-cases
describeSC(Grosche2011)

## Write descriptive statistics to .csv-file
study <- describeSC(Waddell2011)
write.csv(study$descriptives, file = "descriptives.csv")
}