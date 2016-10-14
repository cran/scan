

pand <- function(data, decreasing = FALSE, correction = TRUE) {
  data <- .SCprepareData(data)
  
  phase.expected <- list()
  phase.real <- list()
  A <- list()
  B <- list()
  N <- length(data)
  for (i in 1:N) {
    A[[i]] <- data[[i]][2][data[[i]][1] == "A"]
    B[[i]] <- data[[i]][2][data[[i]][1] == "B"]
    if(class(data[[i]][[1]]) != "factor")
      data[[i]][[1]] <- factor(data[[i]][[1]])
    phase.real[[i]] <- as.numeric(data[[i]][order(data[[i]][2]),1])
    phase.expected[[i]] <- as.numeric(data[[i]][[1]])
    
  }	
  
  tmp <- getOption("warn")
  options(warn = -1)
  results.cor <- cor.test(unlist(phase.real), unlist(phase.expected), method = "kendall")
  options(warn = tmp)
  
  nA <- 0
  nB <- 0
  for (i in 1:N) {
    nA <- nA + length(A[[i]])
    nB <- nB + length(B[[i]])
  }
  
  n <- nA + nB
  
  OD.PP <- rep(NA,N)
  OD.A <- 0
  OD.B <- 0
  for (i in 1:N) {
    z <- data[[i]][2]
    n1 <- length(A[[i]])
    n2 <- length(B[[i]])
    n12 <- n1 + n2
    
    rang <- order(z, decreasing = decreasing)
    AB <- sum(rang[1:n1] > n1, na.rm = TRUE)
    BA <- sum(rang[(n1+1):n12] <= n1, na.rm = TRUE)
    if(correction) {
      ord <- z[rang,]
      AB <- AB + 0.5*sum(ord[1:n1] == min(ord[(n1+1):n12], na.rm = TRUE), na.rm = TRUE)
      BA <- BA + 0.5*sum(ord[(n1+1):n12] == max(ord[1:n1], na.rm = TRUE), na.rm = TRUE)
    }
    OD.PP[i] <- AB + BA
    OD.A <- OD.A + AB
    OD.B <- OD.B + BA
  }
  
  OD <- sum(OD.PP)
  POD <- OD / n * 100
  pA <- nA / n
  pB <- nB / n
  
  b <- OD.A / n
  c <- OD.B / n
  a <- pA - b
  d <- pB - c
  phi <- (a/(a + c))-(b/(b + d))
  PAND <- 100 - POD
  mat <- matrix(c(a,b,c,d), nrow = 2)
  mat2 <- mat * n
  out <- list(PAND = PAND, phi = phi, POD = POD, OD.PP = OD.PP, OD = OD, n = n, N = N, nA = nA, nB = nB, pA = pA, pB = pB, matrix = mat, matrix.counts = mat2, correlation = results.cor, correction = correction)
  class(out) <- c("sc","PAND")
  out
}

