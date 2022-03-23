#' Write data into a .csv-file
#' 
#' This function restructures and writes single-case data into a .csv-file.
#' 
#' This is just a simple wrapper for 
#' `write_csv(as.data.frame(data), filename, row.names = FALSE)`.
#'
#' @inheritParams .inheritParams
#' @param filename A character string defining the output file name (e.g.
#' \code{"SC_data.csv"}.
#' @param sep The field separator string. Values within each row of x are 
#' separated by this string.
#' @param dec The string to use for decimal points in numeric or complex 
#' columns: must be a single character.
#' @param \dots Further arguments passed to write.table.
#' @author Juergen Wilbert
#' @seealso \code{\link{write.table}}, \code{\link{read_scdf}},
#' \code{\link{saveRDS}}
#' @keywords manip
#' @examples
#' ## write single-case data to a .csv-file
#' filename <- file.path(tempdir(), "test.csv")
#' jessica <- random_scdf(design(level = .5))
#' write_scdf(jessica, filename)
#' 
#' ## write multiple cases to a .csv-file with semicolon as field and comma as 
#' ## decimal separator
#' write_scdf(Grosche2011, filename, sep = ";", dec = ",")
#' 
#' ## read_scdf and write_scdf
#' write_scdf(exampleA1B1A2B2_zvt, filename)
#' dat <- read_scdf(filename, cvar = "case", pvar = "part", 
#'                  dvar = "zvt", mvar = "day")
#' res1 <- describe(exampleA1B1A2B2_zvt)$descriptives
#' res2 <- describe(dat)$descriptives
#' all.equal(res1,res2)
#' @export
write_scdf <- function(data, filename = NULL, sep = ",", dec = ".", ...) {
  
  utils::write.table(
    as.data.frame(data), 
    file = filename, 
    row.names = FALSE,
    sep = sep,
    dec = dec,
    ...
  )
  
}

#' @rdname write_scdf
#' @export
writeSC <- function(...) {
  write_scdf(...)
}