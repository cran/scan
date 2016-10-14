

rCi <- function(...){rciSC(...)}

rciSC <- function(data, rel = 0.80, ci = 0.95, graph = FALSE) {
  data <- .SCprepareData(data)
  N <- length(data)
  if(N > 1)
    stop("Multiple single-cases are given. Calculations can only be applied to one single-case data set.\n")
  A <- lapply(data, function(x) x[,2][x[,1] == "A"])
  B <- lapply(data, function(x) x[,2][x[,1] == "B"])
  A <- unlist(A)
  B <- unlist(B)
  sA <- sd(A, na.rm = TRUE)
  sB <- sd(B, na.rm = TRUE)
  mA <- mean(A, na.rm = TRUE)
  mB <- mean(B, na.rm = TRUE)
  nA <- length(A)
  nB <- length(B)
  n <- nA + nB
  SE.A <- sA * sqrt(1 - rel)
  SE.B <- sB * sqrt(1 - rel)
  stand.dif <- (mB-mA)/sd(c(A,B))
  RCI.1 <- (mB - mA) / SE.A
  RCI.2 <- (mB - mA) / sqrt(2*SE.A*SE.A)
  RCI.3 <- (mB - mA)*rel + (mB - mA)*(1-rel) / (sqrt(rel)*sqrt(2*SE.A*SE.A))
  descriptives.ma <- matrix(c(nA,nB, mA,mB,sA,sB, SE.A, SE.B),2,4, dimnames = list(c("A-Phase","B-Phase"),c("n","mean", "SD", "SE"))) 
  z <- qnorm(ci+0.5*(1-ci))
  ci.ma <- matrix(NA, 2,2, byrow = TRUE, dimnames = list(c("A-Phase","B-Phase"),c("Lower", "Upper")))
  ci.ma[1,1] <- mA - z * SE.A
  ci.ma[1,2] <- mA + z * SE.A
  ci.ma[2,1] <- mB - z * SE.B
  ci.ma[2,2] <- mB + z * SE.B
  
  if(graph) {
    dat <- cbind(ci.ma[1,], ci.ma[2,])
    colnames(dat) <- c("A-Phase", "B-Phase")
    main <- sprintf("%d%% confidence interval (rtt = %.2f)", ci*100, rel)
    boxplot(dat, ylab = "Mean", main = main)
  }
  
  RCI.ma <- matrix(c(RCI.1, RCI.2, RCI.3), 3,1,dimnames = list(c("Jacobson et al.","Christensen and Mendoza","Hageman and Arrindell"), "RCI"))
  out <- list(RCI = RCI.ma, stand.dif = stand.dif, conf = ci.ma, conf.percent = ci, reliability = rel, descriptives = descriptives.ma, N = N, A = A, B = B) 
  class(out) <- c("sc","rci")
  
  out
}
