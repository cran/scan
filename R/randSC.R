

rand.test <- function(...) {randSC(...)}

randSC <- function (data, statistic = "Mean B-A", number = 500, complete = FALSE,limit = 5, startpoints = NA, exclude.equal = FALSE, graph = FALSE, output = "c") {
  
  data <- .SCprepareData(data)
  
  a <- lapply(data, function(x) x[,2][x[,1] == "A"])
  b <- lapply(data, function(x) x[,2][x[,1] == "B"])
  obs <- lapply(data, function(x) x[,2])
  MT <- lapply(data, nrow)
  N <- length(data)
  obs.B.start <- unlist(lapply(a, function(x) length(x)+1))
  
  if(is.na(startpoints[1])) {
    pos.startpts <- lapply(MT, function(x) (limit[1]+1):(x-limit+1))
  } else {
    pos.startpts <- lapply(MT, function(x) startpoints)
  }
  
  possible.combinations <- cumprod(unlist(lapply(pos.startpts, length)))[N]	
  
  auto.corrected.number <- FALSE
  if(!complete && possible.combinations <= number) {
    auto.corrected.number <- TRUE
    complete <- TRUE
  }
  
  if(!complete) {
    startpts <- matrix(unlist(lapply(pos.startpts, function(x) sample(x, number, replace = TRUE))), nrow = number, ncol = N)
  }
  if(complete) {
    startpts <- expand.grid(pos.startpts)
    number <- nrow(startpts)
  }
  
  rnd.a <- list()
  for (i in 1:number) {
    ascores <- list()
    for (case in 1:N)
      ascores[[case]] <- data[[case]][1:(startpts[i, case] - 1), 2]
    rnd.a[[i]] <- ascores
  }
  
  rnd.b <- list()
  for (i in 1:number) {
    ascores <- list()
    for (case in 1:N)
      ascores[[case]] <- data[[case]][startpts[i, case]:MT[[case]], 2]
    rnd.b[[i]] <- ascores
  }
  
  
  if (statistic == "B-A" || statistic == "Mean B-A") {
    means.b <- unlist(lapply(rnd.b, function(x) lapply(x,mean,na.rm = TRUE)))
    means.a <- unlist(lapply(rnd.a, function(x) lapply(x,mean,na.rm = TRUE)))
    ma <- matrix(means.b-means.a, ncol = N, nrow = number, byrow = TRUE)
    
    dist <- apply(ma,1,mean,na.rm = TRUE)
    means.b <- unlist(lapply(b, mean,na.rm = TRUE))
    means.a <- unlist(lapply(a, mean,na.rm = TRUE))
    ma <- matrix(means.b-means.a, ncol = N, nrow = 1, byrow = TRUE)
    obs.stat <- mean(ma,na.rm = TRUE)
  }
  
  if (statistic == "A-B" || statistic == "Mean A-B") {
    means.b <- unlist(lapply(rnd.b, function(x) lapply(x,mean,na.rm = TRUE)))
    means.a <- unlist(lapply(rnd.a, function(x) lapply(x,mean,na.rm = TRUE)))
    ma <- matrix(means.a-means.b, ncol = N, nrow = number, byrow = TRUE)
    
    dist <- apply(ma,1,mean,na.rm = TRUE)
    means.b <- unlist(lapply(b, mean,na.rm = TRUE))
    means.a <- unlist(lapply(a, mean,na.rm = TRUE))
    ma <- matrix(means.a-means.b, ncol = N, nrow = 1, byrow = TRUE)
    obs.stat <- mean(ma,na.rm = TRUE)
  }
  
  if (statistic == "Median B-A") {
    medians.b <- unlist(lapply(rnd.b, function(x) lapply(x,median,na.rm = TRUE)))
    medians.a <- unlist(lapply(rnd.a, function(x) lapply(x,median,na.rm = TRUE)))
    ma <- matrix(medians.b-medians.a, ncol = N, nrow = number, byrow = TRUE)
    
    dist <- apply(ma,1,median,na.rm = TRUE)
    medians.b <- unlist(lapply(b, median,na.rm = TRUE))
    medians.a <- unlist(lapply(a, median,na.rm = TRUE))
    ma <- matrix(medians.b-medians.a, ncol = N, nrow = 1, byrow = TRUE)
    obs.stat <- median(ma,na.rm = TRUE)
  }
  
  if (statistic == "Median A-B") {
    medians.b <- unlist(lapply(rnd.b, function(x) lapply(x,median,na.rm = TRUE)))
    medians.a <- unlist(lapply(rnd.a, function(x) lapply(x,median,na.rm = TRUE)))
    ma <- matrix(medians.a-medians.b, ncol = N, nrow = number, byrow = TRUE)
    
    dist <- apply(ma,1,median,na.rm = TRUE)
    medians.b <- unlist(lapply(b, median,na.rm = TRUE))
    medians.a <- unlist(lapply(a, median,na.rm = TRUE))
    ma <- matrix(medians.a-medians.b, ncol = N, nrow = 1, byrow = TRUE)
    obs.stat <- median(ma,na.rm = TRUE)
  }	
  
  
  if (statistic == "Mean |A-B|") {
    means.b <- unlist(lapply(rnd.b, function(x) lapply(x,mean,na.rm = TRUE)))
    means.a <- unlist(lapply(rnd.a, function(x) lapply(x,mean,na.rm = TRUE)))
    ma <- matrix(abs(means.a-means.b), ncol = N, nrow = number, byrow = TRUE)
    
    dist <- apply(ma,1,mean,na.rm = TRUE)
    means.b <- unlist(lapply(b, mean,na.rm = TRUE))
    means.a <- unlist(lapply(a, mean,na.rm = TRUE))
    ma <- matrix(abs(means.a-means.b), ncol = N, nrow = 1, byrow = TRUE)
    obs.stat <- mean(ma,na.rm = TRUE)
  }
  
  
  if (!exclude.equal)
    test <- dist >= obs.stat
  else
    test <- dist > obs.stat
  
  p.value <- sum(test)/number
  
  ### return
  
  if (output == "p") 
    return(p.value)
  
  
  if (graph){
    h <- hist(dist, plot = FALSE)
    lab <- paste0(round(h$counts/length(dist)*100,0), "%")
    xlim <- c(min(h$breaks,na.rm = TRUE), max(h$breaks,na.rm = TRUE))
    if(obs.stat < xlim[1]) 
      xlim[1] <- obs.stat
    if(obs.stat > xlim[2]) 
      xlim[2] <- obs.stat
    hist(dist, xlab = statistic, labels = lab, xlim = xlim, ylab = "Frequency", main = "Random distribution", col = "grey")
    abline(v = obs.stat, lty = 2, lwd = 2) 
    if (p.value < 0.5) pos <- 2 else pos <- 4
    text(obs.stat, max(h$counts,na.rm = TRUE), "observed value", pos = pos)
  }
  
  Z <- (obs.stat - mean(dist,na.rm = TRUE)) / sd(dist,na.rm = TRUE)
  p.Z.single <- 1 - pnorm(Z)
  
  if (output == "c") {
    possible.combinations <- cumprod(unlist(lapply(pos.startpts, length)))[N]
    
    out <- list(statistic = statistic, N = N, n1 = length(unlist(a)), n2 = length(unlist(b)), limit = limit, startpoints = startpoints, p.value = p.value, number = number, complete = complete, observed.statistic = obs.stat, Z = Z, p.Z.single = p.Z.single, distribution = dist, possible.combinations = possible.combinations, auto.corrected.number = auto.corrected.number)	
    class(out) <- c("sc","rand")
    out
  }
}

