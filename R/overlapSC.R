overlapSC <- function(data, decreasing = FALSE) {
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
    d.f$PND[i] <- pnd(data, decreasing = decreasing)$PND
    d.f$PEM[i] <- pem(data, decreasing = decreasing, binom.test = FALSE, chi.test = FALSE)$PEM
    d.f$PET[i] <- pet(data, decreasing = decreasing)$PET
    d.f$NAP[i] <- nap(data, decreasing = decreasing)$NAP
    d.f$NAP.rescaled[i] <- nap(data, decreasing = decreasing)$NAP.rescaled
    d.f$PAND[i] <- pand(data, decreasing = decreasing)$PAND
    d.f$TAU_U[i] <- tauUSC(data)$tau_u
  }
  
  #if(N > 1) {
  #  N <- N + 1
  #  d.f[N,"tmp"] <- NA
  #  d.f$PND[N] <- mean(d.f$PND,na.rm = TRUE)
  #  d.f$PEM[N] <- mean(d.f$PEM,na.rm = TRUE)
  #  d.f$PET[N] <- mean(d.f$PET,na.rm = TRUE)
  #  d.f$NAP[N] <- mean(d.f$NAP,na.rm = TRUE)
  #  d.f$NAP.rescaled[N] <- mean(d.f$NAP.rescaled,na.rm = TRUE)
  #  d.f$PAND[N] <- mean(d.f$PAND,na.rm = TRUE)
  #  d.f$TAU_U[N] <- mean(d.f$TAU_U,na.rm = TRUE)
  #  rownames(d.f) <- c(case.names, "total")
  #}
  #if(N == 1)
    rownames(d.f) <- c(case.names)
  
  out <- list(overlap = d.f[,-1])
  class(out) <- c("sc","overlap")
  out
}
