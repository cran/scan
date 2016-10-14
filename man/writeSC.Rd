\name{writeSC}
\alias{writeSC}
\title{Export data into a .csv-file}
\description{
This function restructures and exports single-case data into a .csv-file.
}

\usage{
writeSC(dat, filename = "", sep = ",", dec = ".", ...)
}

\arguments{
  \item{dat}{A single-case data frame or a list of single-case data frames. See \code{\link{makeSCDF}} to learn about this format.}
  \item{filename}{A character string defining the output file name (e.g. \code{"SC_data.csv"}.}
  \item{sep}{The field separator string. Values within rows will be separated by this string. Default is \code{sep = ","}.}
  \item{dec}{The string used for decimal points. Must be a single character. Default is \code{dec = "."}}
  \item{\dots}{Further arguments passed to write.table.}
}

\author{Juergen Wilbert}

\seealso{\code{\link{write.table}}, \code{\link{readSC}}, \code{\link{makeSCDF}}}

\examples{
## Write single-case data to a .csv-file
jessica <- rSC(d.level = .5)
writeSC(jessica, "SCdata_Jessica.csv")

## Write multiple cases to a .csv-file with semicolon as field and comma as decimal separator
writeSC(Grosche2011, "MBDdata_Grosche.csv", sep = ";", dec = ",")
}

\keyword{manip}

\concept{Export}