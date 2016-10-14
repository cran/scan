

hplm <- function(data, model = "B&L-B", method = "ML", control = list(opt = "optim"), random.slopes = TRUE, ICC = TRUE) {
  dat <- .SCprepareData(data)
  N <- length(dat)
  for(i in 1:N) {
    n1 <- sum(dat[[i]]$phase == "A")
    dat[[i]]$D <- ifelse(dat[[i]]$phase == "A",0,1)
    for(j in 1:nrow(dat[[i]])) {
      if(model == "H-M")
        dat[[i]]$inter[j] <- (dat[[i]]$mt[j]-dat[[i]]$mt[n1+1])*dat[[i]]$D[j]
      if(model == "B&L-B")
        dat[[i]]$inter[j] <- (dat[[i]]$mt[j]-dat[[i]]$mt[n1])*dat[[i]]$D[j]
    }
  }
  dat <- longSCDF(dat)
  out <- list()
  out$model <- model
  out$method <- method
  out$N <- N
  out$analyze.random.slopes <- random.slopes
  out$analyze.ICC <- ICC
  if(!random.slopes)
    out$random.intercept$model <- lme(values ~ mt + D + inter, random =~1|case, data = dat, na.action=na.omit, method = method, control=control)
  if(random.slopes) {
    #out$random.trend$model <- lme(values ~ mt + D + inter, random =~ 1 + mt|case, data = dat, na.action=na.omit, method = method, control=control)
    #out$random.level$model <- lme(values ~ mt + D + inter, random =~ 1 + D|case, data = dat, na.action=na.omit, method = method, control=control)
    #out$random.slope$model <- lme(values ~ mt + D + inter, random =~ 1 + inter|case, data = dat, na.action=na.omit, method = method, control=control)
    out$random.trend.level$model <- lme(values ~ mt + D + inter, random =~ 1 + mt + D |case, data = dat, na.action=na.omit, method = method, control=control)
    out$random.trend.slope$model <- lme(values ~ mt + D + inter, random =~ 1 + mt + inter|case, data = dat, na.action=na.omit, method = method, control=control)
    out$random.level.slope$model <- lme(values ~ mt + D + inter, random =~ 1 + D + inter|case, data = dat, na.action=na.omit, method = method, control=control)
    out$random.trend.level.slope$model <- lme(values ~ mt + D + inter, random =~ 1 + mt + D + inter|case, data = dat, na.action=na.omit, method = method, control=control)
    out$random.nointercept.trend.level.slope$model <- lme(values ~ mt + D + inter, random =~ -1 + mt + D + inter|case, data = dat, na.action=na.omit, method = method, control=control)
    out$random.trend$LR.test <- anova(out$random.level.slope$model, out$random.trend.level.slope$model)
    out$random.level$LR.test <- anova(out$random.trend.slope$model, out$random.trend.level.slope$model)
    out$random.slope$LR.test <- anova(out$random.trend.level$model, out$random.trend.level.slope$model)
    out$random.nointercept.trend.level.slope$LR.test <- anova(out$random.nointercept.trend.level.slope$model, out$random.trend.level.slope$model)
    
  }
  if(ICC) {
    out$model.0 <- lme(values ~ 1, random =~1|case, data = dat, method = method, na.action=na.omit, control = control)
    VC <- as.numeric(VarCorr(out$model.0))
    out$ICC <- VC[1]/(VC[1]+VC[2])	
    out$model.without <- gls(values ~ 1, data = dat, method = method, na.action=na.omit, control = control)
    dif <- anova(out$model.0, out$model.without)
    out$L.ICC <- dif$L.Ratio[2]
    out$p.ICC <- dif$"p-value"[2]
  } 
  
  class(out) <- c("sc","hplm")
  
  out
}
