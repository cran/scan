

pet <- function(data, ci = 0.95, decreasing = FALSE) {
  
  data <- .SCprepareData(data)
  
  N <- length(data)
  
  if (ci != 0) se.factor <- qnorm(ci) else se.factor <- 0 
  
  pet <- rep(NA, N)
  pet.ci <- rep(NA, N)
  p <- rep(NA, N)
  for(i in 1:N) {
    model <- lm(values~mt, data = data[[i]][data[[i]][,1] == "A",], na.action = na.omit)
    B <- data[[i]][data[[i]][,1] == "B",]
    res <- predict(model, B, se.fit = TRUE)
    nB <- nrow(B)
    if(!decreasing) {
      pet.ci[i] <- mean(B$values > (res$fit + res$se.fit * se.factor), na.rm = TRUE)*100
      pet[i] <- mean(B$values > res$fit, na.rm = TRUE)*100
      p[i] <- binom.test(sum(B$values > res$fit, na.rm=TRUE), nB, alternative = "greater")$p.value
    } else {
      pet.ci[i] <- mean(B$values < (res$fit - res$se.fit * se.factor), na.rm = TRUE)*100
      pet[i] <- mean(B$values < res$fit, na.rm = TRUE)*100
      p[i] <- binom.test(sum(B$values < res$fit, na.rm = TRUE), nB, alternative = "less")$p.value
    }
  }
  if(is.null(names(data)))
    names(data) <- paste("Case",1:N)
  
  out <- list(PET = pet, PET.ci = pet.ci, p = p, ci.percent = ci * 100, se.factors = se.factor, N = N, decreasing = decreasing, case.names = names(data))
  class(out) <- c("sc","PET")
  out
}
