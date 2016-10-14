

makesingleSC <- function(data, scale = FALSE, type = "add") {
  warning("This function is deprecated. Please don't use it anymore.\n")
  data <- .SCprepareData(data)
  N <- length(data)
  for(i in 1:N) {
    m  <- mean(data[[i]][data[[i]][,1] == "A",2], na.rm = TRUE)
    if (!scale) 
      sd <- 1
    if (scale) 
      sd <-  sd(data[[i]][data[[i]][,1] == "A",2], na.rm = TRUE)
    data[[i]][,2] <- (data[[i]][,2] - m) / sd 
    A <- data[[i]][data[[i]][,1] == "A",]	
    B <- data[[i]][data[[i]][,1] == "B",]	
    B[,3] <- B[,3] - min(B[,3], na.rm = TRUE) + 1
    if(i == 1) {
      new.data.A <- A
      new.data.B <- B	
    }
    if(i > 1) {
      new.data.A <- rbind(new.data.A, A)	
      new.data.B <- rbind(new.data.B, B)	
    }
  }
  new.data.A <- new.data.A[order(new.data.A[,3]),]
  if(type == "mean") {
    tmp <- aggregate(values~mt, data = new.data.A, mean, na.rm = TRUE)
    new.data.A <- data.frame(phase = rep("A", nrow(tmp)), values = tmp$values, mt = tmp$mt)
    tmp <- aggregate(values~mt, data = new.data.B, mean, na.rm = TRUE)
    new.data.B <- data.frame(phase = rep("B", nrow(tmp)), values = tmp$values, mt = tmp$mt)
  }
  if(type == "median") {
    tmp <- aggregate(values~mt, data = new.data.A, median, na.rm = TRUE)
    new.data.A <- data.frame(phase = rep("A", nrow(tmp)), values = tmp$values, mt = tmp$mt)
    tmp <- aggregate(values~mt, data = new.data.B, median, na.rm = TRUE)
    new.data.B <- data.frame(phase = rep("B", nrow(tmp)), values = tmp$values, mt = tmp$mt)
  }
  maxA <- max(new.data.A[,3], na.rm = TRUE)
  new.data.B[,3] <- new.data.B[,3] + maxA
  new.data.B <- new.data.B[order(new.data.B[,3]),]
  new.data <- rbind(new.data.A, new.data.B)
  
  return(list(new.data))
}
