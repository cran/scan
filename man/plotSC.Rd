\name{plotSC}
\alias{plotSC}
\title{Plot single-case data}
\description{
The \code{plotSC} function provides a plot of a single-case or multiple single-cases.
}

\usage{
plotSC(data, ylim = NULL, xlim = NULL, fill = "", lines = "", marks = NULL,
       annotations = NULL, phase.names = c("A", "B"), FUN.AB = NULL, 
       xlab = "Measurement time", ylab = "Score", text.ABlag = NULL, lwd = 2, 
       pch = 17, type = "b", mai = c(0.6, 0.82, 0.2, 0.42), ...)
}

\arguments{
  \item{data}{A single-case data frame or a list of single-case data frames. See \code{\link{makeSCDF}} to learn about this format.}
  \item{ylim}{Lower and upper limits of the y-axis (e.g., \code{ylim = c(0, 20)} sets the y-axis to a scale from 0 to 20). With multiple single-cases you can use \code{ylim = c(0, NA)} to scale the y-axis from 0 to the maximum of each case. \code{ylim} is not set by default, which makes \code{scan} set a proper scale based on the given data.}
  \item{xlim}{Lower and upper limits of the x-axis (e.g., \code{xlim = c(0, 20)} sets the x-axis to a scale from 0 to 20). With multiple single-cases you can use \code{ylim = c(0, NA)} to scale the x-axis from 0 to the maximum of each case. \code{xlim} is not set by default, which makes \code{scan} set a proper scale based on the given data.}
  \item{fill}{If set, the area under the line is filled with the given color (e.g., \code{fill = "tomato"}). Use the standard R command colors() to get a list of all possible colours. \code{fill} is empty by default.}
  \item{lines}{A character or list defining one or more lines or curves to be plotted. The argument is either passed as a character string (e.g., \code{lines = "median"}) or as a list (e.g., \code{list("median", "trend")}. Some of the procedures can be refined with an additional argument (e.g., \code{lines = list("mean" = 0.20)} adds a 20\% trimmed mean line. By default no additional lines are plotted. Possible lines are:
    \itemize{
      \item{\code{"median"} Separate lines for phase A and B medians.}
      \item{\code{"mean"} Separate lines for phase A and B means. By default it is 10\%-trimmed. Other trims can be set, using a second parameter (e.g., \code{lines = list(mean = 0.2)} draws a 20\%-trimmed mean line).}
      \item{\code{"trend"} Separate lines for phase A and B trends.}
      \item{\code{"trendA"} Trend line for phase A, extrapolated throughout phase B.}
      \item{\code{"maxA"} Line at the level of the highest phase A score.}
      \item{\code{"medianA"} Line at the phase A median score.}
      \item{\code{"meanA"} Line at the phase A 10\%-trimmed mean score. Apply a different trim, by using the additional argument (e.g., \code{lines = list(meanA = 0.2)}).}
      \item{\code{"plm"} Regression lines for piecewise linear regression model.}
      \item{\code{"plm.ar"} Regression lines for piecewise autoregression model. The lag is specified like this: \code{lines = list(plm.ar = 2)}.}
      \item{\code{"movingMean"} Draws a moving mean curve, with a specified lag:
        \code{lines = list(movingMean = 2)}. Default is a lag 1 curve.}
      \item{\code{"movingMedian"} Draws a moving median curve, with a specified lag:
        \code{lines = list(movingMedian = 3)}. Default is a lag 1 curve.}
      \item{\code{"loreg"} Draws a non-parametric local regression line. The proportion of data influencing each data point can be specified using
        \code{lines = list("loreg" = 0.66)}. The default is 0.5.}
      \item{\code{"lty"} Use this argument to define the line type. Examples are: \code{"solid"}, \code{"dashed"}, \code{"dotted"}.}
      \item{\code{"lwd"} Use this argument to define the line's thickness, e.g., \code{lwd = 4}.}
      \item{\code{"col"} Use this argument to define the line's color, e.g., \code{col = "red"}.}
    }
  }
  \item{marks}{A list of parameters defining markings of certain data points.
    \itemize{
      \item{\code{"positions"} A vector or a list of vectors indicating measurement-times to be highlighted. In case of a vector, the marked measurement-times are the same for all plotted cases. In case of a list of vectors, marks are set differently for each case. The list must have the same length as there are cases in the data file.}
      \item{\code{"col"} Color of the marks.}
      \item{\code{"cex"} Size of the marks.}
    }
    Use for example \code{marks = list(positions = c(1, 8, 15), col = "red", cex = 3)} to make the MTs one, eight and 18 appear big and red.
  }
  \item{annotations}{A list of parameters defining annotations to each data point. This adds the score of each MT to your plot.
    \itemize{
      \item{\code{"pos"} Position of the annotations: 1 = below, 2 = left, 3 = above, 4 = right.}
      \item{\code{"col"} Color of the annotations.}
      \item{\code{"cex"} Size of the annotations.}
      \item{\code{"round"} Rounds the values to the specified decimal.}
    }
    \code{annotations = list(pos = 3, col = "brown", round = 1)} adds scores rounded to one decimal above the data point in brown color to the plot.
  }
  \item{phase.names}{By default phases are labeled 'A' and 'B'. Use this argument to specify different labels: \code{phase.names = c("Baseline", "Intervention")}.}
 \item{FUN.AB}{Not in use.}
  \item{xlab}{The label of the x-axis. Default is \code{xlab = "Measurement time"}.}
  \item{ylab}{The labels of the y-axis. Default is \code{ylab = "Score"}.}
  \item{text.ABlag}{By default a vertical line separates phases A and B in the plot. Alternatively, you could print a character string between the two phases using this argument: \code{textAB.lag = "Start"}.}
  \item{lwd}{Width of the plot line. Default is \code{lwd = 2}.}
  \item{pch}{Point type. Default is \code{pch = 17} (triangles). Other options are for example: 16 (filled circles) or "A" (uses the letter A).}
  \item{type}{Line type. "l" draws lines, "p" points, "b" draws lines and points, and "n" draws nothing. Default is \code{type = "b"}). The "n" argument is useful in combination with the lines argument (e.g., \code{type = "n", lines = "loreg"}).}
  \item{mai}{Sets the margins of the plot.}
  \item{...}{Further arguments passed to the plot command.}
}

\value{Returns a plot of one or multiple single-cases.}

\author{Juergen Wilbert}

\seealso{\code{\link{describeSC}}, \code{\link{overlapSC}}}

\examples{
## Request the default plot of the data from Borckhardt (2014)
plotSC(Borckardt2014)

## Plot the three cases from Grosche (2011) and visualize the phase A trend
plotSC(Grosche2011, fill = "tomato", lines = "trendA")

## Request the local regression line for Georg from that data set and customize the plot
plotSC(Grosche2011$Georg, fill = "grey", type = "n", ylim = c(0,NA),
       xlab = "Training session", ylab = "Words per minute",
       phase.names = c("Baseline", "Intervention"), 
       lines = list("loreg", lty = "solid", col = "black", lwd = 3))

## Plot a random MBD over three cases and mark interesting MTs
dat <- rSC(3)
plotSC(dat, marks = list(positions = list(c(2,4,5),c(1,2,3),c(7,8,9)), col = "blue",
       cex = 1.4),annotations = list(label = "values","col" = "red", cex = 0.75,
       offset = 1, round = 0))
}

\concept{Plot}
\concept{Visualization}