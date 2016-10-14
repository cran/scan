

trendSC <- function(data, B.offset = -1,model = NA) {
  phase <- NULL
  data <- .SCprepareData(data)
  
  N <- length(data)
  if(N > 1)
    stop("Multiple single-cases are given. Calculations can only be applied to one single-case data set.\n")
  
  data <- data[[1]]
  
  data.A <- subset(data, phase == "A")
  data.B <- subset(data, phase == "B")
  data.B$mt <- data.B$mt - min(data.B$mt) + 1 + B.offset
  
  row.names <- c("Linear.AB","Linear.A","Linear.B","Squared.AB","Squared.A","Squared.B")
  rows <- length(row.names)
  
  out <- c(
    .SCbeta(lm(values~mt, data = data)), 
    .SCbeta(lm(values~mt, data = data.A)),
    .SCbeta(lm(values~mt, data = data.B)),
    .SCbeta(lm(values~I(mt^2), data = data)),
    .SCbeta(lm(values~I(mt^2), data = data.A)),
    .SCbeta(lm(values~I(mt^2), data = data.B))
  )
  
  if(!is.na(model[1])) {
    for(i in 1:length(model)) {
      out <- c(out,
               .SCbeta(lm(as.formula(model[i]), data = data)),
               .SCbeta(lm(as.formula(model[i]), data = data.A)),
               .SCbeta(lm(as.formula(model[i]), data = data.B))
      )
    }
    rows <- rows + length(model) * 3
    if(is.null(names(model)))
      names(model) <- model
    row.names <- c(row.names,paste(rep(names(model), each = 3), rep(c("AB","A","B")), sep = "."))
  }
  
  out <- matrix(out,rows,3, byrow = TRUE, dimnames = list(row.names, c("Intercept", "B","Beta")))
  out <- list(trend = out, B.offset = B.offset)
  class(out) <- c("sc","trend")
  out
}

