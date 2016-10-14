

smoothSC <- function(data, FUN = "movingMedian", intensity = NULL){
  data <- .SCprepareData(data)
  if (FUN == "movingMean") {
    if(is.null(intensity)) intensity <- 1
    return(lapply(data, function(x) {
      x[,2] <- .SCmovingAverage(x[,2], intensity, mean)
      x}))
  }
  if (FUN == "movingMedian") {
    if(is.null(intensity)) intensity <- 1
    return(lapply(data, function(x) {
      x[,2] <- .SCmovingAverage(x[,2], intensity, median)
      x}))
  }
  if (FUN == "localRegression") {
    if(is.null(intensity)) intensity <- 0.2
    return(lapply(data, function(x) {
      xval <- x[,3]
      xval <- xval[!is.na(x[,2])]
      yval <- x[!is.na(x[,2]),2]
      x[,2] <- lowess(yval~xval, f = intensity)$y
      x}))
  }
}	
