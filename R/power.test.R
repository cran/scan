### experimetal functions

.power.testSC <- function(n = NA, MT = NA, B.start = NA, d.level = NA, d.slope = NA, cases = NA, d.trend = NA, extreme.p = 0, rand.test.stat, extreme.d = c(-4,-3),rtt = NA, alpha = NA, m = NA, s = NA, limit = NA, startpoints = NA, exclude.equal = NA, concise = NA, return.distribution = FALSE, stat, test.parameter, distribution) {
  
  out <- list()
  out$rand.level <- NA
  out$plm.slope <- NA
  out$plm.level <- NA
  out$plm.poisson.slope <- NA
  out$plm.poisson.level <- NA
  
  out$rand.sample <- NA
  
  rand.sample <- rSC(n = n, MT = MT, m = m, s = s,B.start = B.start, d.level = d.level, d.slope = d.slope, cases = cases, d.trend = d.trend, extreme.p = extreme.p, extreme.d = extreme.d, rtt = rtt, concise = concise, distribution = distribution)
  if(cases == 1)
    rand.sample <- lapply(rand.sample, list)
  
  if(any(stat == "rand.test")) {
    if(any(test.parameter == "level")) {
      p.rand.level <- unlist(lapply(rand.sample, function(x) randSC(x, statistic = rand.test.stat[1], number = 100, exclude.equal = exclude.equal, limit = limit, startpoints = startpoints, output = "p")))
      out$rand.level <- mean(p.rand.level <= alpha, na.rm = TRUE)
    } else out$rand.level <- NA
  }
  
  if(any(stat == "plm")) {
    if(any(test.parameter == "level")) {
      p.plm.level <- unlist(lapply(rand.sample, function(x) .plm.mt(x, type = "level p")))
      out$plm.level <- mean(p.plm.level <= alpha, na.rm = TRUE)
    } else out$plm.level <- NA
    
    
    if(any(test.parameter == "slope")) {
      p.plm.slope <- unlist(lapply(rand.sample, function(x) .plm.mt(x, type = "slope p")))
      out$plm.slope <- mean(p.plm.slope <= alpha, na.rm = TRUE)
    } else out$plm.slope <- NA
  }
  
  if(any(stat == "plm.poisson")) {
    if(any(test.parameter == "level")) {
      p.plm.poisson.level <- unlist(lapply(rand.sample, function(x) .plm.mt(x, count.data = TRUE, type = "level p")))
      out$plm.poisson.level <- mean(p.plm.poisson.level <= alpha, na.rm = TRUE)
    } else out$plm.poisson.level <- NA
    
    
    if(any(test.parameter == "slope")) {
      p.plm.poisson.slope <- unlist(lapply(rand.sample, function(x) .plm.mt(x, count.data = TRUE, type = "slope p")))
      out$plm.poisson.slope <- mean(p.plm.poisson.slope <= alpha, na.rm = TRUE)
    } else out$plm.poisson.slope <- NA
  }
  
  
  if(any(stat == "hplm")) {
    if(any(test.parameter == "level")) {
      p.hplm.level <- unlist(lapply(rand.sample, function(x) summary(hplm(x, random.slopes = FALSE, ICC = FALSE)$random.intercept$model)$tTable[3,5]))
      out$hplm.level <- mean(p.hplm.level <= alpha, na.rm = TRUE)
    } else out$hplm.level <- NA
    
    
    if(any(test.parameter == "slope")) {
      p.hplm.slope <- unlist(lapply(rand.sample, function(x) summary(hplm(x, random.slopes = FALSE, ICC = FALSE)$random.intercept$model)$tTable[4,5]))
      out$hplm.slope <- mean(p.hplm.slope <= alpha, na.rm = TRUE)
    } else out$hplm.slope <- NA
  }
  
  
  if(return.distribution)
    out$rand.sample <- rand.sample
  
  out
}

power.testSC <- function(data = NULL, stat = c("rand.test","plm"), test.parameter = c("level", "slope"), rand.test.stat = c("Mean B-A","B"), cases = NULL, rtt = 0.8, d.level = NULL, d.slope = NULL, MT = NULL, B.start = NULL, d.trend = NULL, n = 100, limit = 5,  m = NULL, s = NULL, startpoints = NA, extreme.p = 0, extreme.d = c(-4,-3), exclude.equal = "auto", alpha = 0.05, distribution = "normal", concise = TRUE, silent = FALSE) {
  
  return.distribution <- FALSE # depricated parameter 	
  
  if(!is.null(data)) {
    data <- .SCprepareData(data)
    cases <- length(data)
    B.start <- unlist(lapply(data, function(x) sum(x$phase == "A") + 1))
    MT <- unlist(lapply(data, function(x) length(x$mt)))
    
    d.level <- c()
    d.slope <- c()
    d.trend <- c()
    m <- c()
    
    for(i in 1:cases) {
      res <- coef(plm(data[i])$full)
      m <- c(m,res[1])
      d.trend <- c(d.trend,res[2])
      d.level <- c(d.level,res[3])
      d.slope <- c(d.slope,res[4])
    }
    if(cases == 2 && is.null(s))
      stop("Standard deviation could not be estimated with less than two cases. Please provide a value.\n")
    if(cases > 2)
      s <- sd(m, na.rm = TRUE)
    d.level <- d.level / s
    d.slope <- d.slope / s
    d.trend <- d.trend / s
    
  }
  if(is.null(data)) {
    if(is.null(d.level)) d.level <- 0
    if(is.null(d.slope)) d.slope <- 0
    if(is.null(d.trend)) d.trend <- 0
    if(is.null(cases)) cases <- 1
    if(is.null(m)) m <- 50
    if(is.null(s)) s <- 10
    
    
  }
  
  if(cases == 1 && is.null(s))
    stop("Standard deviation could not be estimated with less than two cases. Please provide a value.\n")
  if (any(stat %in% c("plm","plm.poissonm")) && cases > 1)
    stop("plm models can not be calculated with more than one case. Consider using hplm\n")
  
  if(exclude.equal == "auto") 
    exclude.equal <- ifelse(cases == 1, TRUE, FALSE)
  
  if(!silent)	{
    cat("Compute Monte-Carlo power-analyses with the following parameters:\n\n")
    cat("Stats:\t\t",stat,"\n")
    cat("Sample studies\t",n,"\n")
    cat("Cases per sample",cases,"\n")
    cat("M\t\t",m,"\n")
    cat("SD\t\t",s,"\n")
    cat("MT\t\t",MT,"\n")
    cat("B.start\t\t",sort(unique(B.start)),"\n")
    cat("rtt\t\t",rtt,"\n")
    cat("d level\t\t",d.level,"\n")
    cat("d slope\t\t",d.slope,"\n")
    cat("d trend\t\t",d.trend,"\n")	
    cat("Extreme.p\t",extreme.p,"\n")	
    cat("Extreme.d\t",extreme.d,"\n")	
    cat("Alpha level\t",alpha,"\n")	
    cat("Exclude equal\t",exclude.equal,"\n")
    if (is.na(startpoints[1])) {
      cat("Limit\t\t",limit,"\n")
    } else {
      cat("Startpoints\t\t",startpoints,"\n")
    }
  }
  out <- list()
  
  out$power.rand.level <- NA
  out$power.plm.level <- NA
  out$power.plm.slope <- NA
  out$power.plm.poisson.level <- NA
  out$power.plm.poisson.slope <- NA
  
  out$power.hplm.level <- NA
  out$power.hplm.slope <- NA
  
  out$alphaerror.rand.level <- NA
  out$alphaerror.plm.level <- NA
  out$alphaerror.plm.slope <- NA
  out$alphaerror.plm.poisson.level <- NA
  out$alphaerror.plm.poisson.slope <- NA
  
  out$alphaerror.hplm.level <- NA
  out$alphaerror.hplm.slope <- NA
  
  out$rand.test.stat <- rand.test.stat
  out$rand.sample <- NA
  
  res <- .power.testSC(n = n, MT = MT, B.start = B.start, rand.test.stat = rand.test.stat, d.level = d.level, d.slope = d.slope, extreme.p = extreme.p, extreme.d = extreme.d,m = m, s = s, cases = cases, d.trend = d.trend, rtt = rtt, alpha = alpha, limit = limit,  startpoints = startpoints, exclude.equal = exclude.equal, concise = concise, return.distribution = return.distribution, stat = stat, test.parameter = test.parameter, distribution = distribution)
  
  if(return.distribution) 
    out$rand.sample <- res$rand.sample
  
  if(all(d.level == 0)) {
    if(any(stat == "rand.test"))
      out$alphaerror.rand.level <- res$rand.level
    if(any(stat == "plm"))
      out$alphaerror.plm.level <- res$plm.level
    if(any(stat == "plm.poisson"))
      out$alphaerror.plm.poisson.level <- res$plm.poisson.level
    if(any(stat == "hplm"))
      out$alphaerror.hplm.level <- res$hplm.level
    
  } else {
    if(any(stat == "rand.test"))
      out$power.rand.level <- res$rand.level
    if(any(stat == "plm"))
      out$power.plm.level <- res$plm.level
    if(any(stat == "plm.poisson"))
      out$power.plm.poisson.level <- res$plm.poisson.level
    if(any(stat == "hplm"))
      out$power.hplm.level <- res$hplm.level
    
  }
  
  
  if(all(d.slope == 0)) {
    if(any(stat == "plm"))
      out$alphaerror.plm.slope <- res$plm.slope
    if(any(stat == "plm.poisson"))
      out$alphaerror.plm.poisson.slope <- res$plm.poisson.slope
    if(any(stat == "hplm"))
      out$alphaerror.hplm.slope <- res$hplm.slope
  } else {
    if(any(stat == "plm"))
      out$power.plm.slope <- res$plm.slope
    if(any(stat == "plm.poisson"))
      out$power.plm.poisson.slope <- res$plm.poisson.slope
    if(any(stat == "hplm"))
      out$power.hplm.slope <- res$hplm.slope
  }
  
  
  if(any(d.level != 0)) {
    res <- .power.testSC(n = n, MT = MT, B.start = B.start, rand.test.stat = rand.test.stat, d.level = 0, d.slope = d.slope, extreme.p = extreme.p, extreme.d = extreme.d,m = m, s = s, cases = cases, d.trend = d.trend, rtt = rtt, alpha = alpha, limit = limit,  startpoints = startpoints, exclude.equal = exclude.equal, concise = concise, stat = stat, test.parameter = test.parameter, distribution = distribution)
    if(any(stat == "rand.test"))
      out$alphaerror.rand.level <- res$rand.level
    if(any(stat == "plm"))
      out$alphaerror.plm.level <- res$plm.level
    if(any(stat == "plm.poisson"))
      out$alphaerror.plm.poisson.level <- res$plm.poisson.level
    if(any(stat == "hplm"))
      out$alphaerror.hplm.level <- res$hplm.level
    
  }
  
  if(any(d.slope != 0)) {
    res <- .power.testSC(n = n, MT = MT, B.start = B.start, rand.test.stat = rand.test.stat, d.level = d.level, d.slope = 0, extreme.p = extreme.p, extreme.d = extreme.d,m = m, s = s, cases = cases, d.trend = d.trend, rtt = rtt, alpha = alpha, limit = limit,  startpoints = startpoints, exclude.equal = exclude.equal, concise = concise, stat = stat, test.parameter = test.parameter, distribution = distribution)
    if(any(stat == "plm"))
      out$alphaerror.plm.slope <- res$plm.slope
    if(any(stat == "plm.poisson"))
      out$alphaerror.plm.poisson.slope <- res$plm.poisson.slope
    if(any(stat == "hplm"))
      out$alphaerror.hplm.slope <- res$hplm.slope
  }
  
  class(out) <- c("sc","power")
  out
}
