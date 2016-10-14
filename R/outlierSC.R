

outlierSC <- function(data, criteria = c("SD", "2")){
  
  data.list <- .SCprepareData(data)
  
  out <- list()
  
  N <- length(data.list)
  case.names <- names(data.list)
  dropped.mts <- list()
  dropped.n <- list()
  ci.matrix <- list()
  sd.matrix <- list()
  cook <- list()
  
  if(is.null(case.names))
    case.names <- paste("Case", 1:N, sep = "")
  
  for(i in 1:N) {
    data <- data.list[[i]]
    A <- data[,2][data[,1] == "A"]
    B <- data[,2][data[,1] == "B"]
    if (criteria[1] == "CI") {
      cut.off <- as.numeric(criteria[2])
      fac <- qnorm((1-cut.off)/2, lower.tail = FALSE)
      critA.upper <- mean(A) + fac * (sd(A)/sqrt(length(A)))
      critA.lower <- mean(A) - fac * (sd(A)/sqrt(length(A)))
      critB.upper <- mean(B) + fac * (sd(B)/sqrt(length(B)))
      critB.lower <- mean(B) - fac * (sd(B)/sqrt(length(B)))
      filterA <- (A < critA.lower) | (A > critA.upper)
      filterB <- (B < critB.lower) | (B > critB.upper)
      filterAB <- c(filterA, filterB)
      mat <- matrix(c(critA.lower, critB.lower, critA.upper, critB.upper), nrow = 2, dimnames = list(c("A-phase","B-phase"), c("lower", "upper")))
      ci.matrix[[i]] <- mat
    }
    if (criteria[1] == "SD") {
      SD <- as.numeric(criteria[2])
      critA.upper <- mean(A) + SD * sd(A)
      critA.lower <- mean(A) - SD * sd(A)
      critB.upper <- mean(B) + SD * sd(B)
      critB.lower <- mean(B) - SD * sd(B)
      filterA <- (A < critA.lower) | (A > critA.upper)
      filterB <- (B < critB.lower) | (B > critB.upper)
      filterAB <- c(filterA, filterB)
      mat <- matrix(c(critA.lower, critB.lower, critA.upper, critB.upper), nrow = 2, dimnames = list(c("A-phase","B-phase"), c("lower", "upper")))
      sd.matrix[[i]] <- mat
    }		
    if (criteria[1] == "Cook") {
      if (criteria[2] == "4/n")
        cut.off <- 4/(length(A)+length(B))
      else
        cut.off <- as.numeric(criteria[2])
      n1 <- length(A)
      MT <- data[,3]
      values <- data[,2]
      T <- MT[n1+1]
      D <- c(rep(0, length(A)), rep(1, length(B)))
      int <-  D * (MT - T)
      reg <- lm(values ~ 1 + MT + D + int)
      cd <- cooks.distance(reg)
      filterAB <- cd >= cut.off
      cook[[i]] <- data.frame(Cook = round(cd,2), MT = MT)
    }		
    
    #data.list[[i]][,4] <- filterAB
    #names(data.list[[i]])[4] <- "outlier"
    dropped.mts[[i]] <- data.list[[i]]$mt[filterAB]
    dropped.n[[i]] <- sum(filterAB)
    
    data.list[[i]] <- data.list[[i]][!filterAB,]
  }
  
  out$data <- data.list
  out$dropped.mt <- dropped.mts
  out$dropped.n <- dropped.n
  out$ci.matrix <- ci.matrix
  out$sd.matrix <- sd.matrix
  out$cook <- cook
  out$criteria <- criteria
  out$N <- N
  out$case.names <- case.names
  class(out) <- c("sc","outlier")
  out
}
