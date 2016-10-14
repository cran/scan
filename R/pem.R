

pem <- function(data, decreasing = FALSE, binom.test = TRUE, chi.test = FALSE, FUN = median, ...) {
  
  data <- .SCprepareData(data)
  N <- length(data)
  PEM <- rep(NA,N)
  chi <- rep(NA,N)
  chi.df <- rep(NA,N)
  chi.p <- rep(NA,N)
  binom.p <- rep(NA,N)
  
  for(i in 1:N) {
    A <- data[[i]][,2][data[[i]][,1] == "A"]
    B <- data[[i]][,2][data[[i]][,1] == "B"]
    if (!decreasing)
      PEM[i] <- mean(B > FUN(A, na.rm = TRUE,...), na.rm = TRUE) * 100
    if (decreasing)
      PEM[i] <- mean(B < FUN(A, na.rm = TRUE,...), na.rm = TRUE) * 100
    if(binom.test) {
      nB <- length(B)
      binom.p[i] <- binom.test(round(PEM[i] / 100  * nB), nB, alternative = ifelse(decreasing, "less", "greater"))$p.value
    }
    if(chi.test) {
      nB <- length(B)
      exceeding <- PEM[i] / 100  * nB
      res <- chisq.test(c(exceeding, nB - exceeding), p = c(0.5,0.5))
      chi[i] <- res$statistic
      chi.df[i] <- res$parameter
      chi.p[i] <- res$p.value
    }
  }
  stats.ma <- cbind(binom.p,chi, chi.df, chi.p)
  colnames(stats.ma) <- c("binom.p","Chi", "DF", "p")
  if(is.null(names(data)))
    names(data) <- paste("Case",1:N)
  rownames(stats.ma) <- names(data)
  
  out <- list(PEM = PEM, test = stats.ma, decreasing = decreasing)
  class(out) <- c("sc","PEM")
  out
}
