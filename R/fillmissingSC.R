

fillmissingSC <- function(data, interpolation = "linear", na.rm = TRUE) {
  data <- .SCprepareData(data)
  N <- length(data)
  for(i in 1:N) {
    dat <- data[[i]]
    if (na.rm)
      dat <- dat[!is.na(dat$values),]
    new.dat <- dat
    for(j in 1 : (nrow(dat)-1)) {
      if(dat$mt[j+1] - dat$mt[j] != 1){
        if(interpolation == "linear")
          step.size <- (dat$values[j+1] - dat$values[j]) / (dat$mt[j+1] - dat$mt[j])
        for(k in (dat$mt[j]+1) : (dat$mt[j+1]-1)) {
          tmp <- dat[j, ]
          tmp$mt <- k
          if(interpolation == "linear")
            tmp$values <- dat$values[j] + step.size * (k - dat$mt[j])
          new.dat <- rbind(new.dat, tmp) 
        }
      }
    }
    data[[i]] <- new.dat[order(new.dat$mt),]
  }
  data
}
