
readSC <- function(filename, sep = ",", dec = ".", sort.labels = FALSE, phase.names = c("A","B"),...) {
  dat <- read.table(filename, header = TRUE, sep = sep, dec = dec, stringsAsFactors = FALSE, ...)
  columns <- ncol(dat)
  names(dat) <- c("case", "phase", "values", "mt")[1:columns]
  if(!sort.labels) 
    dat$case <- factor(dat$case, levels = unique(dat$case))
  else
    dat$case <- factor(dat$case)
  
  dat$phase[dat$phase == phase.names[1]] <- "A"
  dat$phase[dat$phase == phase.names[2]] <- "B"
  dat$phase <- factor(dat$phase)
  
  lab <- levels(dat$case)
  dat <- split(dat, dat$case)
  dat <- lapply(dat, function(x) x[,2:columns])
  for(i in 1:length(dat))
    row.names(dat[[i]]) <- 1:nrow(dat[[i]])
  names(dat) <- lab
  cat("Imported",length(dat),"cases.\n")
  if(columns == 3) {
    cat("Measurement-times are missing. Standard times were assigned.\n")
    dat <- .SCprepareData(dat)
  }
  return(dat)
}
