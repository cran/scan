
writeSC <- function(dat, filename, sep = ",", dec = ".", ...) {
  write.table(longSCDF(dat), filename, sep = sep, row.names = FALSE, dec = dec, ...)
}
