#' Add a label to a figure. Taken from <https://waterprogramming.wordpress.com/2015/12/02/easy-labels-for-multi-panel-plots-in-r/>
#'
#' @param label Label that is to be drawn
#' @param location Location of the label. Allowed entries are 'topleft',
#'   'topcenter', 'topright', 'bottomleft', 'bottomright', 'bottomcenter'
#' @param offset Coordinates of the offset of the label
#' @keywords internal
figureAddLabel <- function(label, location = "topleft",
                           offset = c(0, 0)) {
  coords <- switch(location,
    topleft = c(0.015, 0.98),
    topcenter = c(0.5525, 0.98),
    topright = c(0.985, 0.98),
    bottomleft = c(0.015, 0.02),
    bottomcenter = c(0.5525, 0.02),
    bottomright = c(0.985, 0.02),
    c(0.015, 0.98)
  )

  this.x <- grconvertX(coords[1] + offset[1], from = "nfc", to = "user")
  this.y <- grconvertY(coords[2] + offset[2], from = "nfc", to = "user")
  text(labels = label[1], x = this.x, y = this.y, xpd = T)
}

#' Add legend to a plot
#'
#' @inheritParams graphics::legend
#' @param ... Additional arguments to be passed to `graphics::legend`.
#' @keywords internal
.figureAddLegend <- function(x, legend, col, pch, lty, ...) {
  # Legend does not ignore ellipsis arguments that are not supported, so they
  # must be removed from the arguments list
  supportedArgs <- names(formals(graphics::legend))
  args <- list(...)
  args <- args[which(names(args) %in% supportedArgs)]
  args$x <- x
  args$legend <- legend
  args$col <- col
  args$pch <- pch
  args$lty <- lty

  # Using do.call use arguments combined from ellipsis
  do.call(graphics::legend, args)
}

#' A function to add error bars on the chart.
#' @description Taken from <https://www.r-graph-gallery.com/4-barplot-with-error-bar.html>
#' @import ospsuite
#'
#' @param x Numerical array of x-values
#' @param y Numerical array of y-values
#' @param upper Numerical array of upper y-error values
#' @param lower Numerical array of lower y-error values. Optional.
#' If not specified, same values as for `upper` are used
#' @param length Numerical value specifying the width of the error bars.
#'   Optional. Default is 0.1.
#' @param axis Dimension to which the error bars are added. If "y" (default),
#'   vertical error bars are drawn. If "x", horizontal error bars are drawn
#' @param ... Graphical parameters (see [par()])
#'
#' @keywords internal
plotErrorBars <- function(x,
                          y,
                          upper,
                          lower = upper,
                          length = par()$cin[[1]] / 2,
                          axis = "y",
                          ...) {
  validateIsNumeric(c(x, y))
  validateIsSameLength(x, y, upper, lower)
  if (axis == "y") {
    arrows(x, y + upper, x, y - lower, angle = 90, code = 3, length = length, ...)
  }
  if (axis == "x") {
    arrows(x + upper, y, x - lower, y, angle = 90, code = 3, length = length, ...)
  }
}

#' Possible entries for the `outputDevice` field of a `PlotConfiguration` object
#' @export
GraphicsDevices <- enum(list("PNG"))

#' Possible entries for the `plotType` field of a `DataMapping` object
#'
#' @details "IndividualProfile" - simulated results are plotted as time-values
#'   series with points connected by lines, with each individual results potted
#'   separately "PopulationQuantiles" - simulated results for a population are
#'   aggregated as median, 95th an 5th percentiles, with median plotted as a
#'   line an upper/lower percentiles plotted as shaded areas
#'   "PredictedVsObserved" -  predicted-versus-observed goodness of fit plot
#' @export
PlotTypes <- enum(list(
  "IndividualProfile",
  "PopulationQuantiles",
  "PredictedVsObserved",
  "BoxPlot"
))

#' Open an output device.
#'
#' @param plotConfiguration An object of type `PlotConfiguration`
#' @param width Width of the output figure
#' @param height Height of the output figure
#'
#' @details If the output of the plot is directed to a file, open the
#'   connection. A list of supported outputs is provided in
#'   `GraphicsDevices`-enum. If the provided output defined in
#'   PlotConfiguration$outputDevice is not supported or the value is `NULL`,
#'   output is directed to the default plot frame.
#'
#' @keywords internal
openOuptutDevice <- function(plotConfiguration, width, height) {
  if (is.null(plotConfiguration$outputDevice)) {
    return()
  }
  if (plotConfiguration$outputDevice == GraphicsDevices$PNG) {
    png(
      filename = file.path(
        plotConfiguration$outputFolder,
        paste0(plotConfiguration$outputName, ".png")
      ),
      width = width,
      height = height,
      units = "cm",
      res = plotConfiguration$res,
      pointsize = plotConfiguration$pointsize
    )
  }
}

#' Close output device
#'
#' @param plotConfiguration An object of type `PlotConfiguration`
#' @import ospsuite
#'
#' @details If the output of the plot is directed to a file, close the device.
#' @keywords internal
closeOutputDevice <- function(plotConfiguration) {
  if (is.null(plotConfiguration$outputDevice)) {
    return()
  }
  if (enumHasKey(plotConfiguration$outputDevice, GraphicsDevices)) {
    dev.off()
  }
}

#' Does the plot type contain points?
#'
#' @param type String value of argument `type` passed to function `plot()`
#'
#' @return TRUE if `type` contains "p" or is "b", FALSE otherwise
#'
#' @keywords internal
.isPoint <- function(type) {
  isCharInString("p", type) || (type == "b")
}

#' Does the plot type contain line?
#'
#' @param type String value of argument `type` passed to function `plot()`
#'
#' @return TRUE if `type` contains "l" or is "b", FALSE otherwise
#'
#' @keywords internal
.isLine <- function(type) {
  isCharInString("l", type) || (type == "b")
}
