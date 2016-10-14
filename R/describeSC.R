

describeSC <- function(data, decreasing = FALSE) {
  data.list <- .SCprepareData(data)
  N <- length(data.list)
  case.names <- names(data.list)
  if (is.null(case.names))
    case.names <- paste("Case",1:N, sep = "")
  
  d.f <- data.frame(tmp = rep(NA,N))
  
  for(i in 1:N) {
    data <- data.list[[i]]
    A <- data[,2][data[,1] == "A"]
    B <- data[,2][data[,1] == "B"]
    #d.f$acA[i] <- .SCac(A,1)
    #d.f$acB[i] <- .SCac(B,1)
    d.f$nA[i] <- length(A)
    d.f$nB[i] <- length(B)
    d.f$nAB[i] <- d.f$nA[i] + d.f$nB[i]
    d.f$misA[i] <- sum(is.na(A),na.rm = TRUE) 
    d.f$misB[i] <- sum(is.na(B),na.rm = TRUE) 
    d.f$misAB[i] <- d.f$misA[i] + d.f$misB[i]
    d.f$mA[i] <- mean(A,na.rm = TRUE)
    d.f$mB[i] <- mean(B,na.rm = TRUE)
    d.f$mdA[i] <- median(A,na.rm = TRUE)
    d.f$mdB[i] <- median(B,na.rm = TRUE)
    d.f$minA[i] <- min(A,na.rm = TRUE)
    d.f$minB[i] <- min(B,na.rm = TRUE)
    d.f$maxA[i] <- max(A,na.rm = TRUE)
    d.f$maxB[i] <- max(B,na.rm = TRUE)
    trend <- trendSC(data)$trend
    d.f$bA[i] <- trend[2,2]
    d.f$bB[i] <- trend[3,2]
    d.f$bC[i] <- trend[1,2]
    d.f$bdif[i] <- d.f$bB[i] - d.f$bA[i]
    d.f$sdA[i] <- sd(A,na.rm = TRUE)
    d.f$sdB[i] <- sd(B,na.rm = TRUE)
    d.f$sdAB[i] <- sd(data[,2],na.rm = TRUE)
    d.f$dif[i] <- d.f$mB[i] - d.f$mA[i]
    d.f$smd1[i] <- d.f$dif[i]/d.f$sdA[i]
    d.f$smd2[i] <- d.f$dif[i]/d.f$sdB[i]
    d.f$smd3[i] <- d.f$dif[i]/d.f$sdAB[i]
    #d.f$PND[i] <- pnd(data, decreasing = decreasing)$PND
    #d.f$PEM[i] <- pem(data, decreasing = decreasing, binom.test = FALSE, chi.test = FALSE)$PEM
    #d.f$NAP[i] <- nap(data, decreasing = decreasing)$NAP
    #d.f$PAND[i] <- pand(data, decreasing = decreasing)$PAND
    #d.f$TAU_U[i] <- tauUSC(data)$tau_u
  }
  
  if(N > 1) {
    N <- N + 1
    d.f[N,"tmp"] <- NA
    d.f$nA[N] <- sum(d.f$nA,na.rm = TRUE)
    d.f$nB[N] <- sum(d.f$nB,na.rm = TRUE)
    d.f$nAB[N] <- sum(d.f$nAB,na.rm = TRUE)
    d.f$misA[N] <- sum(d.f$misA,na.rm = TRUE)
    d.f$misB[N] <- sum(d.f$misB,na.rm = TRUE)
    d.f$misAB[N] <- sum(d.f$misAB,na.rm = TRUE)
    d.f$mA[N] <- mean(d.f$mA,na.rm = TRUE)
    d.f$mB[N] <- mean(d.f$mB,na.rm = TRUE)
    d.f$dif[N] <- mean(d.f$dif,na.rm = TRUE)
    d.f$mdA[N] <- median(d.f$mdA,na.rm = TRUE)
    d.f$mdB[N] <- median(d.f$mdB,na.rm = TRUE)
    d.f$minA[N] <- min(d.f$minA,na.rm = TRUE)
    d.f$minB[N] <- min(d.f$minB,na.rm = TRUE)
    d.f$maxA[N] <- max(d.f$maxA,na.rm = TRUE)
    d.f$maxB[N] <- max(d.f$maxB,na.rm = TRUE)
    d.f$sdA[N] <- sqrt(mean(d.f$sdA^2,na.rm = TRUE))
    d.f$sdB[N] <- sqrt(mean(d.f$sdB^2,na.rm = TRUE))
    d.f$sdAB[N] <- sqrt(mean(d.f$sdAB^2,na.rm = TRUE))
    d.f$smd1[N] <- mean(d.f$smd1,na.rm = TRUE)
    d.f$smd2[N] <- mean(d.f$smd2,na.rm = TRUE)
    d.f$smd3[N] <- mean(d.f$smd3,na.rm = TRUE)
    #d.f$acA[N] <- mean(d.f$acA,na.rm = TRUE)
    #d.f$acB[N] <- mean(d.f$acB,na.rm = TRUE)
    d.f$bA[N] <- mean(d.f$bA,na.rm = TRUE)
    d.f$bB[N] <- mean(d.f$bB,na.rm = TRUE)
    d.f$bC[N] <- mean(d.f$bC,na.rm = TRUE)
    d.f$bdif[N] <- mean(d.f$bdif,na.rm = TRUE)
    #d.f$PND[N] <- mean(d.f$PND,na.rm = TRUE)
    #d.f$PEM[N] <- mean(d.f$PEM,na.rm = TRUE)
    #d.f$NAP[N] <- mean(d.f$NAP,na.rm = TRUE)
    #d.f$PAND[N] <- mean(d.f$PAND,na.rm = TRUE)
    #d.f$TAU_U[N] <- mean(d.f$TAU_U,na.rm = TRUE)
    rownames(d.f) <- c(case.names, "total")
  }
  if(N == 1)
    rownames(d.f) <- c(case.names)
  
  out <- list(descriptives = d.f[,-1])
  class(out) <- c("sc","describe")
  out
}
