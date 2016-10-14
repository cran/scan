
autocorrSC <- function(data, lag.max = 3) {
  data.list <- .SCprepareData(data)
  N <- length(data.list)
  case.names <- names(data.list)
  if (is.null(case.names))
    case.names <- paste("Case",1:N, sep = "")
  VAR <- paste0("lag_",1:lag.max)
  ac <- data.frame(case = rep(case.names, each = 3), phase = rep(c("A","B","AB"), N))
  ac[,VAR] <- NA
  
  for(i in (0:(N-1)*3)) {
    data <- data.list[[(i/3+1)]]
    A <- data[,2][data[,1] == "A"]
    B <- data[,2][data[,1] == "B"]
    if(length(A)-1 < lag.max) lagA <- length(A)-1 else lagA <- lag.max
    if(length(B)-1 < lag.max) lagB <- length(B)-1 else lagB <- lag.max
    if(length(c(A,B))-1 < lag.max) lagAB <- length(c(A,B))-1 else lagAB <- lag.max
    
    
    ac[i+1,VAR[1:lagA]] <- acf(A, lag.max = lagA, plot = FALSE)$acf[-1]
    ac[i+2,VAR[1:lagB]] <- acf(B, lag.max = lagB, plot = FALSE)$acf[-1]
    ac[i+3,VAR[1:lagAB]] <- acf(c(A,B), lag.max = lagAB, plot = FALSE)$acf[-1]
   }

  out <- list(autocorr = ac)
  class(out) <- c("sc","autocorr")
  out
}
