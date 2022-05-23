#' Calculate quantiles for given xy-vectors
#'
#' @param quantiles A numerical vector with quantile values. Default is
#' `c(0.05, 0.5, 0.95)`
#' @param xValues X values, by which y values are grouped
#' @param yValues Values for which the quantiles are calculated
#'
#' @return A list with `xValues` and aggregated `yValues`
#' @export
getQuantilesYData <- function(xValues, yValues, quantiles = c(0.05, 0.5, 0.95)) {
  validateIsNumeric(c(xValues, yValues, quantiles))
  validateIsSameLength(xValues, yValues)
  output <- list()
  # Aggregate time values
  for (quantile in quantiles) {
    aggregatedData <- aggregate(yValues, by = list(xVals = xValues), FUN = quantile, quantile)
    output[[as.character(quantile)]] <- list()
    output[[as.character(quantile)]][["xValues"]] <- aggregatedData$xVals
    output[[as.character(quantile)]][["yValues"]] <- aggregatedData[[2]]
  }

  return(output)
}
