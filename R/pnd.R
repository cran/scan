
pnd <- function(data, decreasing = FALSE) {
  
  data <- .SCprepareData(data)
  
  PND <- c()
  for(i in 1:length(data)) {
    A <- data[[i]][,2][data[[i]][,1] == "A"]
    B <- data[[i]][,2][data[[i]][,1] == "B"]
    if (!decreasing)
      PND[i] <- sum(B > max(A, na.rm = TRUE), na.rm = TRUE) / length(B) * 100
    if (decreasing)
      PND[i] <- sum(B < min(A, na.rm = TRUE), na.rm = TRUE) / length(B) * 100
  }
  out <- list(PND = PND)
  class(out) <- c("sc","PND")
  out
}
