.getPackageVersion <- function() {
  version <- getNamespaceVersion("esqlabsRLegacy")
  return(version)
}

# Environment that holds various global variables and settings for the esqlabsRLegacy,
# It is not exported and should not be directly manipulated by other packages.
esqlabsLegacyEnv <- new.env(parent = emptyenv())

# name of the package. This will be used to retrieve information on the package at run time
esqlabsLegacyEnv$packageName <- "esqlabsRLegacy"

# Version of the package
esqlabsLegacyEnv$packageVersion <- .getPackageVersion()

# Default width of a plot of a single `PlotMapping`
esqlabsLegacyEnv$widthPerPlotMapping <- 8

# Default height of a plot of a single `PlotMapping`
esqlabsLegacyEnv$heightPerPlotMapping <- 8

# Column names to split observed data by
esqlabsLegacyEnv$columnsToSplitDataBy <- c("Group Id", "Gender", "Patient Id", "Dose", "Route", "Molecule", "Organ", "Compartment")

# Column index for x values in observed data files
esqlabsLegacyEnv$XValuesColumn <- 10
# Column index for y values in observed data files
esqlabsLegacyEnv$YValuesColumn <- 11
# Column index for y error values in observed data files
esqlabsLegacyEnv$YErrorColumn <- 12

#' Get the value of a global esqlabsRLegacy setting.
#'
#' @param settingName String name of the setting
#'
#' @return Value of the setting stored in esqlabsLegacyEnv. If the setting does not
#'   exist, an error is thrown.
#' @export
#'
#' @examples
#' getEsqlabsRSetting("packageVersion")
#' getEsqlabsRSetting("widthPerPlotMapping")
getEsqlabsRSetting <- function(settingName) {
  if (!(any(names(esqlabsLegacyEnv) == settingName))) {
    stop(messages$errorEsqlabsRSettingNotFound(settingName))
  }

  obj <- esqlabsLegacyEnv[[settingName]]
  # Evaluate if the object is a function. This is required since some properties are defined as function reference
  if (is.function(obj)) {
    return(obj())
  }

  return(obj)
}
