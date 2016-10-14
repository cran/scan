

plm <- function(data, AR = NULL, model = "B&L-B", count.data = FALSE, family = ifelse(count.data, "poisson", "gaussian"),...) {
  
  data <- .SCprepareData(data)
  
  N <- length(data)
  
  if(N == 1)
    data <- data[[1]]
  
  if(N > 1)
    stop("Procedure could not be applied for more than one case.\nConsider to use the hplm function.")
  
  
  if (!is.null(AR)) {
    return(.plm.ar(data = data, AR = AR, model = model))
  }
  
  
  #### to do multiple baseline
  
  data <- na.omit(data)
  
  ### model definition
  y <- data[,2]
  n1 <- sum(data[,1] == "A")
  n2 <- sum(data[,1] == "B")
  
  if(model == "H-M") {
    MT <- data[,3]
    D <- c(rep(0, n1), rep(1, n2))
    inter <- (MT-MT[n1+1])*D	
  } else if (model == "B&L-B") {
    MT <- data[,3]
    D <- c(rep(0, n1), rep(1, n2))
    inter <- (MT-MT[n1])*D	
  } else if (model == "Mohr#1") {
    MT <- data[,3]
    D <- c(rep(0, n1), rep(1, n2))
    inter <- MT*D	
  } else if (model == "Mohr#2") {
    MT <- data[,3]
    D <- c(rep(0, n1), rep(1, n2))
    inter <- (MT-MT[n1+1])*D
    MT <- MT-MT[n1+1]
  } else if (model == "Manly") {
    MT <- data[,3]
    D <- c(rep(0, n1), rep(1, n2))
    inter <- MT*D
  } else stop("Wrong model definition!\n")
  
  if(family == "nbinomial") {
    #	library(MASS)
    #	full <- glm.nb(y ~ 1 + MT + D + inter)
    #	lr1 <- glm.nb(y ~ 1 + MT + D)
    #	lr2 <- glm.nb(y ~ 1 + MT + inter)
    #	lr3 <- glm.nb(y ~ 1 + D + inter)
  } else {
    full <- glm(y ~ 1 + MT + D + inter, family = family,...)
    lr1 <- glm(y ~ 1 + MT + D, family = family,...)
    lr2 <- glm(y ~ 1 + MT + inter, family = family,...)
    lr3 <- glm(y ~ 1 + D + inter, family = family,...)
  }
  
  full.I <- full$coefficients[[1]]
  full.T <- full$coefficients[[2]]
  full.D <- full$coefficients[[3]]
  full.TxD <- full$coefficients[[4]]
  
  ### inference
  df2.full <- full$df.residual
  QSE <- sum(full$residuals^2, na.rm = TRUE)
  QST <- sum((y-mean(y))^2, na.rm = TRUE)
  MQSA <- (QST - QSE) / 3
  MQSE <- QSE / df2.full
  F.full <- MQSA / MQSE
  p.full <- pf(F.full,3,df2.full, lower.tail = FALSE)
  r2.full <- 1 - (QSE / QST)
  r2.full.adj <- r2.full-(1-r2.full)*(3/(length(y)-3-1))
  
  r2.full <- 1-(var(full$residuals, na.rm = TRUE)/var(y, na.rm = TRUE))
  r2.lr1 <- 1-(var(lr1$residuals, na.rm = TRUE)/var(y, na.rm = TRUE))
  r2.lr2 <- 1-(var(lr2$residuals, na.rm = TRUE)/var(y, na.rm = TRUE))
  r2.lr3 <- 1-(var(lr3$residuals, na.rm = TRUE)/var(y, na.rm = TRUE))
  
  test.slope <- anova(lr1, full)
  p.slope <- test.slope$"Pr(>F)"[2]
  F.slope <- test.slope$F[2]
  ES.slope <- r2.full-r2.lr1
  
  test.level <- anova(lr2, full)
  p.level <- test.level$"Pr(>F)"[2]
  F.level <- test.level$F[2]
  ES.level <- r2.full-r2.lr2
  
  test.trend <- anova(lr3, full)
  p.trend <- test.trend$"Pr(>F)"[2]
  F.trend <- test.trend$F[2]
  ES.trend <- r2.full-r2.lr3
  
  ### output
  out <- list(model = model, F = F.full, df1 = 3, df2 = df2.full, p = p.full, R2 = r2.full, R2.adj = r2.full.adj, n1 = n1, n2 = n2, count.data = count.data, I = full.I, T = full.T, D = full.D, TxD = full.TxD, F.slope = F.slope, p.slope = p.slope, ES.slope = ES.slope, F.level = F.level, p.level = p.level, ES.level = ES.level, F.trend = F.trend, p.trend = p.trend, ES.trend = ES.trend, full.model = full, MT = MT, data = data, N = N, family = family)
  class(out) <- c("sc", "pr")
  out
}


.plm.ar <- function(data, AR = 2, model = "B&L-B") {
  
  data <- .SCprepareData(data)
  
  #### to do multiple baseline
  N <- length(data)
  if(N > 1)
    stop("Multiple single-cases are given. Calculatioins could only be applied to a single data set.\n")
  
  data <- data[[1]]
  
  ### extracting variables
  y <- data[,2]
  n1 <- sum(data[,1] == "A")
  n2 <- sum(data[,1] == "B")
  
  ### descriptive
  if(model == "H-M") {
    MT <- data[,3]
    D <- c(rep(0, n1), rep(1, n2))
    inter <- (MT-MT[n1+1])*D	
  } else if (model == "B&L-B") {
    MT <- data[,3]
    D <- c(rep(0, n1), rep(1, n2))
    inter <- (MT-MT[n1])*D	
  } else if (model == "Mohr#1") {
    MT <- data[,3]
    D <- c(rep(0, n1), rep(1, n2))
    inter <- MT*D	
  } else if (model == "Mohr#2") {
    MT <- data[,3]
    D <- c(rep(0, n1), rep(1, n2))
    inter <- (MT-MT[n1+1])*D
    MT <- MT-MT[n1+1]
  } else if (model == "Manly") {
    MT <- data[,3]
    D <- c(rep(0, n1), rep(1, n2))
    inter <- MT*D
  }	
  
  
  if(AR > 0)
    full <- gls(y ~ 1 + MT + D + inter, correlation=corARMA(p=AR), method="ML")
  if(AR == 0)
    full <- gls(y ~ 1 + MT + D + inter, method="ML")
  
  full.I <- full$coefficients[[1]]
  full.T <- full$coefficients[[2]]
  full.D <- full$coefficients[[3]]
  full.TxD <- full$coefficients[[4]]
  
  ### inference
  df2.full <- full$dims$N - full$dims$p
  QSE <- sum(full$residuals^2, na.rm = TRUE)
  QST <- sum((y-mean(y))^2, na.rm = TRUE)
  MQSA <- (QST - QSE) / 3
  MQSE <- QSE / df2.full
  F.full <- MQSA / MQSE
  p.full <- pf(F.full,3,df2.full, lower.tail = FALSE)
  r2.full <- 1 - (QSE / QST)
  r2.full.adj <- r2.full-(1-r2.full)*(3/(length(y)-3-1))
  
  if(AR > 0) {
    lr1 <- gls(y ~ 1 + MT + D, correlation=corARMA(p=AR), method="ML")
    lr2 <- gls(y ~ 1 + MT + inter, correlation=corARMA(p=AR), method="ML")
    lr3 <- gls(y ~ 1 + D + inter, correlation=corARMA(p=AR), method="ML")
  }
  if(AR == 0) {
    lr1 <- gls(y ~ 1 + MT + D, method="ML")
    lr2 <- gls(y ~ 1 + MT + inter, method="ML")
    lr3 <- gls(y ~ 1 + D + inter, method="ML")
  }
  r2.full <- 1-(var(full$residuals, na.rm = TRUE)/var(y, na.rm = TRUE))
  r2.lr1 <- 1-(var(lr1$residuals, na.rm = TRUE)/var(y, na.rm = TRUE))
  r2.lr2 <- 1-(var(lr2$residuals, na.rm = TRUE)/var(y, na.rm = TRUE))
  r2.lr3 <- 1-(var(lr3$residuals, na.rm = TRUE)/var(y, na.rm = TRUE))
  
  test.slope <- anova(lr1, full)
  p.slope <- test.slope$"Pr(>F)"[2]
  F.slope <- test.slope$F[2]
  ES.slope <- r2.full-r2.lr1
  
  test.level <- anova(lr2, full)
  p.level <- test.level$"Pr(>F)"[2]
  F.level <- test.level$F[2]
  ES.level <- r2.full-r2.lr2
  
  test.trend <- anova(lr3, full)
  p.trend <- test.trend$"Pr(>F)"[2]
  F.trend <- test.trend$F[2]
  ES.trend <- r2.full-r2.lr3
  
  ### output
  out <- list(model = model, F = F.full, df1 = 3, df2 = df2.full, p = p.full, R2 = r2.full, R2.adj = r2.full.adj, n1 = n1, n2 = n2, I = full.I, T = full.T, D = full.D, TxD = full.TxD, F.slope = F.slope, p.slope = p.slope, ES.slope = ES.slope, F.level = F.level, p.level = p.level, ES.level = ES.level, F.trend = F.trend, p.trend = p.trend, ES.trend = ES.trend,full.model = full, MT = MT, data = data, ar = AR, N = N, count.data = FALSE, family = "gaussian")
  class(out) <- c("sc", "plm.ar")
  out
}



.plm.mt <- function(data, type = "level p", model = "B&L-B", count.data = FALSE) {
  N <- length(data)
  if(N > 1)
    stop("Multiple single-cases are given. Calculatioins could only be applied to a single data set.\n")
  
  if(class(data)=="list")
    data <- data[[1]]
  if(ncol(data) < 3)
    data[,3] <- 1:nrow(data)
  
  y <- data[,2]
  n1 <- sum(data[,1] == "A")
  n2 <- sum(data[,1] == "B")
  
  if(model == "H-M") {
    MT <- data[,3]
    D <- c(rep(0, n1), rep(1, n2))
    inter <- (MT-MT[n1+1])*D	
  } else if (model == "B&L-B") {
    MT <- data[,3]
    D <- c(rep(0, n1), rep(1, n2))
    inter <- (MT-MT[n1])*D	
  } else if (model == "Mohr#1") {
    MT <- data[,3]
    D <- c(rep(0, n1), rep(1, n2))
    inter <- MT*D	
  } else if (model == "Mohr#2") {
    MT <- data[,3]
    D <- c(rep(0, n1), rep(1, n2))
    inter <- (MT-MT[n1+1])*D
    MT <- MT-MT[n1+1]
  } else if (model == "Manly") {
    MT <- data[,3]
    D <- c(rep(0, n1), rep(1, n2))
    inter <- MT*D
  }	
  
  if(count.data) {
    full <- glm(I(round(y)) ~ 1 + MT + D + inter, family = "poisson")
  } else full <- lm(y ~ 1 + MT + D + inter)
  
  if (type == "1" || type == "level p")
    return(summary(full)$coef[3,4])
  if (type == "2" || type == "slope p")
    return(summary(full)$coef[4,4])
  if (type == "3" || type == "level t") 
    return(summary(full)$coef[3,3])
  if (type == "4" || type == "slope t")
    return(summary(full)$coef[4,3])
  if (type == "5" || type == "level B")
    return(summary(full)$coef[3,1])
  if (type == "6" || type == "slope B")
    return(summary(full)$coef[4,1])
  if (type == "model")
    return(full)
  
}
