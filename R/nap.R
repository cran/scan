

nap <- function(data, decreasing = FALSE) {
  
  data <- .SCprepareData(data)
  N <- length(data)
  if(N > 1)
    stop("Multiple single-cases are given. Calculations could only be applied to a single data set.\n")
  
  data <- data[[1]]
  
  A <- data[data[,1] == "A",2]
  B <- data[data[,1] == "B",2]
  n1 <- length(A)
  n2 <- length(B)
  
  pairs <- n1 * n2
  if (!decreasing)
    pos <- pairs - sum(sapply(A,function(x)x>=B), na.rm = TRUE)
  if (decreasing)
    pos <- pairs - sum(sapply(A,function(x)x<=B), na.rm = TRUE)
  
  ties <- sum(sapply(A,function(x)x==B), na.rm = TRUE)
  NAP <- (pos + (0.5 * ties)) / pairs 
  
  out <- list(NAP = NAP*100, NAP.rescaled = 2 * (NAP*100) - 100)
  class(out) <- c("sc","NAP")
  out
}
