\name{readSC}
\alias{readSC}
\title{Read single-case data from files}
\description{
Use the \code{readSC} function to import single-case data from structured .csv-files.
}

\usage{
readSC(filename, sep = ",", dec = ".", sort.labels = FALSE,
       phase.names = c("A", "B"), ...)
}

\arguments{
  \item{filename}{A character string defining the file to be imported (e.g. \code{"SC_Anita.csv"}.}
  \item{sep}{The field separator string. Values within rows have to be separated by this string. Default is \code{sep = ","}.}
  \item{dec}{The string used for decimal points in the file. Must be a single character. Default is \code{dec = "."}}
  \item{sort.labels}{If set TRUE, the resulting list is sorted by label names (alphabetically increasing).}
  \item{phase.names}{Name of the phases. Usually they should be "A" and "B". I the file contains different names, these are converted to "A" and "B".}
  \item{\dots}{Further arguments passed to the \code{\link{read.table}} command.}
}

\value{Returns a single-case data frame. See \code{\link{makeSCDF}} to learn about the format of these data frames.}

\author{Juergen Wilbert}

\seealso{\code{\link{read.table}}, \code{\link{writeSC}}, \code{\link{makeSCDF}}}

\examples{
## Read SC-data from a file named "study1.csv" in your working directory
# study1 <- readSC("study1.csv")

## Read SC-data from a .csv-file with semicolon as field and comma as decimal separator
# study2 <- readSC("study2.csv", sep = ";", dec = ",")
}

\keyword{manip}

\concept{Import}