

truncateSC <- function (data, A = c(0,0), B = c(0,0)){
  data <- .SCprepareData(data)
  N = length(data)
  
  for(i in 1:N){
    lengthA <- sum(data[[i]]$phase == "A")
    lengthB <- sum(data[[i]]$phase == "B")
    selectA <- (A[1] + 1):(lengthA - A[2])
    select <- c(selectA,(lengthA + 1 + B[1]):(lengthA + lengthB - B[2]))
    data[[i]] <- data[[i]][select,]
    data[[i]]$mt <- 1:nrow(data[[i]])
  }
  return(data)
}
