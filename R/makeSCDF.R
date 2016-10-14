
makeSCDF <- function (data, B.start = NULL, MT = NULL){
  if (is.null(MT))
    MT <- 1:length(data)
  B.start <- which(MT == B.start)
  D <- c(rep("A", B.start - 1), rep("B", length(data) - B.start + 1))
  data <- data.frame(phase = D, values = data, mt = MT)
  data
}
