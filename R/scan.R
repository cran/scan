
.onAttach <- function(lib, pkg, ...) {
	out <- paste0("scan ",packageVersion("scan"),"\n","Single-Case Data Analysis for Single and Multiple AB-Designs\n",
	              "Caution! This is a beta version and heavily under construction!\n")
	packageStartupMessage(out)
}	

.onLoad <- function(lib, pkg, ...) {}

.SCmovingAverage <- function(x, xLag, FUN = mean) {
	for(i in (xLag + 1):(length(x) - xLag))
		x[i] <- FUN(x[(i - xLag):(i + xLag)], na.rm = TRUE)
	return(x)
}
	
.SCac <- function(x, lag = 1) {
	m <- mean(x, na.rm = TRUE)
	ax1 <- x[1:(length(x) - lag)]-m
	ax2 <- x[(lag + 1):length(x)]-m
	ac <- sum(ax1*ax2, na.rm = TRUE)/sum((x-m)^2, na.rm = TRUE)
	ac
}

.SClm <- function(x = NULL,y) {
	if(is.null(x))
		x <- 1:length(y)
	mx <- mean(x)
	my <- mean(y)
	ss.xy <- sum( (x-mx)*(y-my) )
	ss.xx <- sum( (x-mx)^2 )
	b <- ss.xy/ss.xx
	b
}

.SCbeta <- function(model) {
	  b <- model$coefficients[-1]
    sx <- apply(model$model[-1],2,sd)
    sy <- apply(model$model[1],2,sd)
    return(c(model$coefficients,b * sx/sy))
}

.SCprepareData <- function(data, B.start = NULL, MT = NULL) {
	
	if(is.data.frame(data))
		data <- list(data)

	if(class(data) != "list")
		stop("Wrong data format. Data must be a data frame or a list of data frames.")

	for(i in 1:length(data)) {
		if(ncol(data[[i]]) == 2) {
		  data[[i]]$mt <- 1:nrow(data[[i]])
		  names(data[[i]]) <- c("phase", "values", "mt")
		}
	}
		
	return(data)
}


longSCDF <- function(data) {
  dat <- .SCprepareData(data)
  label <- names(dat)
  if (is.null(label))
    label <- as.character(1:length(dat))
  outdat <- vector()
  for (i in 1:length(dat)) {
	  dat[[i]]$case <- label[i]
		outdat <- rbind(outdat, dat[[i]])
	}
  outdat <- cbind(outdat[,ncol(outdat)],outdat[,-ncol(outdat)])
  colnames(outdat)[1] <- "case"
  return(outdat)
}

.onAttach()



