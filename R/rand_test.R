#' Randomization Tests for single-case data
#'
#' The `rand_test` function computes a randomization test for single or multiple
#' baseline single-case data.  The function is based on an algorithm from the
#' `SCRT` package (Bulte & Onghena, 2009, 2012), but rewritten and extended
#' for the use in AB designs.
#'
#'
#' @inheritParams .inheritParams
#' @param statistic Defines the statistic on which the comparison of phases A
#'   and B is based on. Default setting is `statistic = "Mean B-A"`. The
#' following comparisons are possible: \itemize{ \item\code{"Mean A-B"}: Uses
#' the difference between the mean of phase A and the mean of phase B. This is
#' appropriate if a decrease of scores was expected for phase B.
#' \item\code{"Mean B-A"}: Uses the difference between the mean of phase B and
#' the mean of phase A. This is appropriate if an increase of scores was
#' expected for phase B.  \item\code{"Mean |A-B|"}: Uses the absolute value of
#' the difference between the means of phases A and B.  \item\code{"Median
#' A-B"}: The same as \code{"Mean A-B"}, but based on the median.
#' \item\code{"Median B-A"}: The same as \code{"Mean B-A"}, but based on the
#' median.  }
#' @param number Sample size of the randomization distribution. The exactness of
#'   the p-value can not exceed \eqn{1/number} (i.e., `number = 100` results in
#'   p-values with an exactness of one percent). Default is `number = 500`. For
#'   faster processing use `number = 100`. For more precise p-values set `number
#'   = 1000`).
#' @param complete If TRUE, the distribution is based on a complete permutation
#'   of all possible starting combinations. This setting overwrites the number
#'   Argument. The default setting is FALSE.
#' @param limit Minimal number of data points per phase in the sample. The first
#'   number refers to the A-phase and the second to the B-phase (e.g., `limit =
#'   c(5,3)`). If only one number is given, this number is applied to both
#'   phases. Default is `limit = 5`.
#' @param startpoints Alternative to the `limit`-parameter `startpoints` exactly
#'   defines the possible start points of phase B (e.g., `startpoints = 4:9`
#'   restricts the phase B start points to measurements 4 to 9. `startpoints`
#'   overruns the `limit`-parameter.
#' @param exclude.equal If set to \code{exclude.equal = FALSE}, which is the
#'   default, random distribution values equal to the observed distribution are
#'   counted as null-hypothesis conform. That is, they decrease the probability
#'   of rejecting the null-hypothesis (increase the p-value).
#'   \code{exclude.equal} should be set to `TRUE` if you analyse one single-case
#'   design (not a multiple baseline data set) to reach a sufficient power. But
#'   be aware, that it increases the chance of an alpha-error.
#' @param graph If `graph = TRUE`, a histogram of the resulting distribution is
#'   plotted. It is `FALSE` by default. *Note: use the more versatile
#'   [plot_rand()] function instead.*
#' @param output (deprecated and not implemented)
#' @param seed A seed number for the random generator.
#' @return \item{statistic}{Character string from function call (see
#'   \code{Arguments} above).} \item{N}{Number of single-cases.}
#'   \item{n1}{Number of data points in phase A.} \item{n2}{Number of data
#'   points in phase B.} \item{limit}{Numeric from function call (see
#'   \code{Arguments} above).}
#' \item{startpoints}{A vector defining the start points passed from the
#' function call (see \code{Arguments} above).} \item{p.value}{P-value of the
#' randomization test for the given data.} \item{number}{Sample size of
#' randomization distribution from function call (see \code{Arguments} above).}
#' \item{complete}{Logical argument from function call (see \code{Arguments}
#' above).} \item{observed.statistic}{Test statistic observed for the given
#' single-case data. (see \code{statistic} in the \code{Arguments} above.)}
#' \item{Z}{Z-value of observed test statistic.} \item{p.z.single}{Probability
#' of z-value.} \item{distribution}{Test statistic distribution from randomized
#' data sets.} \item{possible.combinations}{Number of possible combinations
#' under the given restrictions.} \item{auto.corrected.number}{\code{TRUE}
#' indicates that a corrected number of combinations was used. This happens, if
#' the number of possible combinations (under the given restrictions) undercuts
#' the requested \code{number} of combinations.}
#' @author Juergen Wilbert
#' @references Bulte, I., & Onghena, P. (2009). Randomization tests for
#'   multiple-baseline designs: An extension of the SCRT-R package.
#' *Behavior Research Methods, 41*, 477-485.
#'
#'   Bulte, I., & Onghena, P. (2012). *SCRT: Single-Case Randomization Tests*.
#'   Available from: \url{https://CRAN.R-project.org/package=SCRT}
#' @examples
#'
#' ## Compute a randomization test on the first case of the byHeart2011 data and include a graph
#' rand_test(byHeart2011[1], statistic = "Median B-A", graph = TRUE, seed = 123)
#'
#' ## Compute a randomization test on the Grosche2011 data using complete permutation
#' rand_test(Grosche2011, statistic = "Median B-A", complete = TRUE, limit = 4, seed = 123)
#'
#' @export
rand_test <- function (data, dvar, pvar, 
                       statistic = c("Mean B-A", "Mean A-B", "Median B-A", 
                                     "Median A-B", "Mean |A-B|", "Median |A-B|"), 
                       number = 500, 
                       complete = FALSE, 
                       limit = 5, 
                       startpoints = NA, 
                       exclude.equal = FALSE, 
                       phases = c(1, 2), 
                       graph = FALSE, 
                       output = NULL, 
                       seed = NULL) {

  
  check_args(
    by_call(statistic, "rand_test")
  )
  statistic <- statistic[1]
  
  if(!is.null(seed)) set.seed(seed)
  
  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- dv(data)
  if (missing(pvar)) pvar <- phase(data) 
  dv(data) <- dvar
  phase(data) <- pvar
  
  data <- .prepare_scdf(data)
  
  keep <- recombine_phases(data, phases = phases)
  data <- keep$data
  
  a   <- lapply(data, function(x) x[x[, pvar] == "A", dvar])
  b   <- lapply(data, function(x) x[x[, pvar] == "B", dvar])
  obs <- lapply(data, function(x) x[, dvar])
  MT  <- lapply(data, nrow)
  N   <- length(data)
  
  if (identical(exclude.equal, "auto")) exclude.equal <- N == 1
  
# starting points ---------------------------------------------------------

  if (length(limit) == 1) limit[2] <- limit[1]
  obs.B.start <- unlist(lapply(a, function(x) length(x) + 1))
  
  if (is.na(startpoints[1])) {
    pos.startpts <- lapply(MT, function(x) (limit[1] + 1):(x - limit[2] + 1))
  } else {
    pos.startpts <- lapply(MT, function(x) startpoints)
  }
  
  ### posible combinations
  
  possible.combinations <- lapply(pos.startpts, length)
  possible.combinations <- cumprod(unlist(possible.combinations))[N]	
  
  auto.corrected.number <- FALSE
  if (!complete && possible.combinations <= number) {
    auto.corrected.number <- TRUE
    complete <- TRUE
  }
  
  if (!complete) {
    startpts <- lapply(pos.startpts, function(x) sample(x, number, replace = TRUE))
    startpts <- matrix(unlist(startpts), nrow = number, ncol = N)
  }
  if (complete) {
    startpts <- expand.grid(pos.startpts)
    number   <- nrow(startpts)
  }
  
# Sample Random A and B phases ---------------------------------------------
  
  rnd.a <- list()
  for (i in 1:number) {
    ascores <- list()
    for (case in 1:N)
      ascores[[case]] <- data[[case]][1:(startpts[i, case] - 1), dvar]
    rnd.a[[i]] <- ascores
  }
  
  rnd.b <- list()
  for (i in 1:number) {
    ascores <- list()
    for (case in 1:N)
      ascores[[case]] <- data[[case]][startpts[i, case]:MT[[case]], dvar]
    rnd.b[[i]] <- ascores
  }
  
# Functions for phase differences -----------------------------------------

  if (statistic == "B-A" || statistic == "Mean B-A") {
    means.b <- unlist(lapply(rnd.b, function(x) lapply(x, mean,na.rm = TRUE)))
    means.a <- unlist(lapply(rnd.a, function(x) lapply(x, mean,na.rm = TRUE)))
    ma <- matrix(means.b - means.a, ncol = N, nrow = number, byrow = TRUE)
    
    dist <- apply(ma, 1, mean, na.rm = TRUE)
    means.b <- unlist(lapply(b, mean,na.rm = TRUE))
    means.a <- unlist(lapply(a, mean,na.rm = TRUE))
    ma <- matrix(means.b - means.a, ncol = N, nrow = 1, byrow = TRUE)
    obs.stat <- mean(ma,na.rm = TRUE)
  }
  
  if (statistic == "A-B" || statistic == "Mean A-B") {
    means.b <- unlist(lapply(rnd.b, function(x) lapply(x, mean,na.rm = TRUE)))
    means.a <- unlist(lapply(rnd.a, function(x) lapply(x, mean,na.rm = TRUE)))
    ma <- matrix(means.a - means.b, ncol = N, nrow = number, byrow = TRUE)
    
    dist <- apply(ma, 1, mean,na.rm = TRUE)
    means.b <- unlist(lapply(b, mean, na.rm = TRUE))
    means.a <- unlist(lapply(a, mean, na.rm = TRUE))
    ma <- matrix(means.a - means.b, ncol = N, nrow = 1, byrow = TRUE)
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
    medians.b <- unlist(lapply(rnd.b, function(x) lapply(x, median, na.rm = TRUE)))
    medians.a <- unlist(lapply(rnd.a, function(x) lapply(x, median, na.rm = TRUE)))
    ma <- matrix(medians.a - medians.b, ncol = N, nrow = number, byrow = TRUE)
    
    dist <- apply(ma, 1, median,na.rm = TRUE)
    medians.b <- unlist(lapply(b, median, na.rm = TRUE))
    medians.a <- unlist(lapply(a, median, na.rm = TRUE))
    ma <- matrix(medians.a - medians.b, ncol = N, nrow = 1, byrow = TRUE)
    obs.stat <- median(ma, na.rm = TRUE)
  }	
  
  
  if (statistic == "Mean |A-B|") {
    means.b <- unlist(lapply(rnd.b, function(x) lapply(x, mean, na.rm = TRUE)))
    means.a <- unlist(lapply(rnd.a, function(x) lapply(x, mean, na.rm = TRUE)))
    ma <- matrix(abs(means.a - means.b), ncol = N, nrow = number, byrow = TRUE)
    
    dist <- apply(ma, 1, mean,na.rm = TRUE)
    means.b <- unlist(lapply(b, mean, na.rm = TRUE))
    means.a <- unlist(lapply(a, mean, na.rm = TRUE))
    ma <- matrix(abs(means.a - means.b), ncol = N, nrow = 1, byrow = TRUE)
    obs.stat <- mean(ma, na.rm = TRUE)
  }

  if (statistic == "Median |A-B|") {
    medians.b <- unlist(lapply(rnd.b, function(x) lapply(x, median, na.rm = TRUE)))
    medians.a <- unlist(lapply(rnd.a, function(x) lapply(x, median, na.rm = TRUE)))
    ma <- matrix(abs(medians.a - medians.b), ncol = N, nrow = number, byrow = TRUE)
    dist <- apply(ma, 1, median,na.rm = TRUE)
    
    medians.b <- unlist(lapply(b, median, na.rm = TRUE))
    medians.a <- unlist(lapply(a, median, na.rm = TRUE))
    ma <- matrix(abs(medians.a - medians.b), ncol = N, nrow = 1, byrow = TRUE)
    obs.stat <- median(ma, na.rm = TRUE)
  }

# p value -----------------------------------------------------------------

  if (!exclude.equal) test <- dist >= obs.stat else test <- dist > obs.stat
  
  p.value <- sum(test) / number
  
# return ------------------------------------------------------------------

  if (graph){
    h <- hist(dist, plot = FALSE)
    lab <- paste0(round(h$counts / length(dist) * 100, 0), "%")
    xlim <- c(min(h$breaks,na.rm = TRUE), max(h$breaks, na.rm = TRUE))
    ylim <- round(max(h$counts * 1.2))
    if (obs.stat < xlim[1]) xlim[1] <- obs.stat
    if (obs.stat > xlim[2]) xlim[2] <- obs.stat
    hist(
      dist, xlab = statistic, labels = lab, xlim = xlim, ylim = c(0, ylim), 
      ylab = "Frequency", main = "Random distribution", col = "lightgrey"
    )
    abline(v = obs.stat, lty = 2, lwd = 2, col = "grey") 
    if (p.value < 0.5) pos <- 2 else pos <- 4
    text(obs.stat, ylim, "observed", pos = pos)
  }
  
  Z <- (obs.stat - mean(dist, na.rm = TRUE)) / sd(dist, na.rm = TRUE)
  p.Z.single <- 1 - pnorm(Z)
  possible.combinations <- cumprod(unlist(lapply(pos.startpts, length)))[N]

  out <- list(
    statistic = statistic, 
    phases.A = keep$phases_A, 
    phases.B = keep$phases_B, 
    N = N, 
    n1 = length(unlist(a)), 
    n2 = length(unlist(b)), 
    limit = limit, 
    startpoints = startpoints, 
    p.value = p.value, 
    number = number, 
    complete = complete, 
    observed.statistic = obs.stat, 
    Z = Z, 
    p.Z.single = p.Z.single, 
    distribution = dist, 
    possible.combinations = possible.combinations, 
    auto.corrected.number = auto.corrected.number
  )
  
  class(out) <- c("sc_rand")
  attr(out, opt("phase")) <- pvar
  attr(out, opt("dv")) <- dvar
  attr(out, "casenames") <- names(data)
  out

}

#' @rdname deprecated-functions
#' @export
randSC <- function(...) {
  .deprecated_warning("rand_test", "randSC")
  rand_test(...)
}

#' @rdname deprecated-functions
#' @export
rand.test <- function(...) {
  .deprecated_warning("rand_test", "rand.test")
  rand_test(...)
}


