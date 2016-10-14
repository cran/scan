

tauUSC <- function (data, ties.method = "omit") {
  data <- .SCprepareData(data)
  N <- length(data)
  if(N == 1)
    data <- data[[1]]
  
  if(N > 1)
    stop("Procedure could not be applied for more than one case.\n")
  
  out <- matrix(rep(NA,6*5),6,5, dimnames= list(c("A vs. B","Trend A","Trend B","Full","A vs. B + Trend B", "A vs. B + Trend B - Trend A"),c("pairs", "pos", "neg","S","Tau")))
  
  out <- as.data.frame(out)
  
  A <- data[data[,1] == "A",2]
  B <- data[data[,1] == "B",2]
  
  AB <- c(A,B)
  nA <- length(A)
  nB <- length(B)
  nAB <- nA+nB
  
  tau_m <- matrix(NA, nrow = nAB, ncol = nAB, dimnames = (list(AB,AB)))
  tmp <- t(sapply(AB,function(x) x > AB))
  tau_m[tmp] <- "-"
  tmp <- t(sapply(AB,function(x) x < AB))
  tau_m[tmp] <- "+"
  tmp <- t(sapply(AB,function(x) x == AB))
  tau_m[tmp] <- "T"
  
  diag(tau_m) <- 0
  
  pos.s <- c("+")
  neg.s <- c("-")
  if(ties.method == "positive") 
    pos.s <- c("+","T")
  if(ties.method == "negative") 
    neg.s <- c("-","T")
  
  
  tau_m[lower.tri(tau_m)] <- ""
  
  AvBm <- tau_m[1:nA,(nA+1):nAB]
  AvBpos <- sum(AvBm%in%pos.s)
  AvBneg <- sum(AvBm%in%neg.s)
  
  AvAm <- tau_m[1:nA,1:nA]
  AvApos <- sum(AvAm%in%pos.s)
  AvAneg <- sum(AvAm%in%neg.s)
  
  BvBm <- tau_m[(nA+1):nAB,(nA+1):nAB]
  BvBpos <- sum(BvBm%in%pos.s)
  BvBneg <- sum(BvBm%in%neg.s)
  
  out$pairs <- c(nA*nB, (nA*(nA-1))/2, (nB*(nB-1))/2, (nAB*(nAB-1))/2, nA*nB + (nB*(nB-1))/2, (nAB*(nAB-1))/2)
  out$pos <- c(AvBpos,AvApos,BvBpos,AvApos+BvBpos+AvBpos, AvBpos+BvBpos, AvBpos+BvBpos+AvAneg)
  out$neg <- c(AvBneg,AvAneg,BvBneg,AvAneg+BvBneg+AvBneg, AvBneg+BvBneg, AvBneg+BvBneg+AvApos)
  out$S <- out$pos-out$neg
  out$Tau <- out$S / out$pairs
  out <- list(table = out, matrix = tau_m, tau_u = out$Tau[6])
  class(out) <- c("sc","TAU-U")
  out
}
