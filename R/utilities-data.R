#' Read time-values data from excel file
#'
#' @param dataConfiguration An object of `DataConfiguration`
#' @details The methods reads time-values data from the properly defined excel
#'   sheet and creates `XYData` objects according to the groupings. Each sheet
#'   in `DataConfiguration$sheets` is split according to columns listed in
#'   `DataConfiguration$columnsToSplitBy`. The output structure is a nested list
#'   with levels corresponding to the groupings.
#'
#' @return A (nested) list of `XYData` objects
#' @import ospsuite esqlabsR
#' @export
readOSPSTimeValues <- function(dataConfiguration) {
  validateIsString(c(dataConfiguration$dataFolder, dataConfiguration$dataFile, dataConfiguration$sheets))
  filePath <- file.path(dataConfiguration$dataFolder, dataConfiguration$dataFile)
  validateFileExists(filePath)

  observedData <- list()
  for (sheet in dataConfiguration$dataSheets) {
    data <- readExcel(path = filePath, sheet = sheet)
    allFactors <- list()
    groupings <- c()
    # Split the data by a column only if it contains non-NA values
    for (columnName in dataConfiguration$columnsToSplitBy) {
      if (length(data[[columnName]]) > 0 && all(!is.na(data[[columnName]]))) {
        groupings <- c(groupings, columnName)
        allFactors <- append(allFactors, list(data[[columnName]]))
      }
    }
    data <- split(data, allFactors, drop = TRUE)

    # Create an XYData object for each group
    for (groupIdx in seq_along(data)) {
      group <- data[[groupIdx]]
      groupName <- names(data)[[groupIdx]]
      xVals <- group[[dataConfiguration$XValuesColumn]]
      yVals <- group[[dataConfiguration$YValuesColumn]]
      yErrorVals <- group[[dataConfiguration$YErrorColumn]]

      # Parse Dimensions and Units
      xName <- colnames(group)[[dataConfiguration$XValuesColumn]]
      yName <- colnames(group)[[dataConfiguration$YValuesColumn]]
      yErrorName <- colnames(group)[[dataConfiguration$YErrorColumn]]


      # Get name of the dimension before the unit
      xDim <- strsplit(xName, "\\ ?\\[")[[1]][[1]]
      # The unit is the second entry
      xUnit <- strsplit(xName, "\\ ?\\[")[[1]][[2]]
      # Remove the trailing ']'
      xUnit <- gsub(pattern = "]", replacement = "", xUnit, fixed = TRUE)

      yDim <- strsplit(yName, "\\ ?\\[")[[1]][[1]]
      yUnit <- strsplit(yName, "\\ ?\\[")[[1]][[2]]
      yUnit <- gsub(pattern = "]", replacement = "", yUnit, fixed = TRUE)

      yErrorUnit <- strsplit(yErrorName, "\\ ?\\[")[[1]][[2]]
      yErrorUnit <- gsub(pattern = "]", replacement = "", yErrorUnit, fixed = TRUE)

      timeValues <- XYData$new(stringToNum(xVals), stringToNum(yVals), label = paste(sheet, groupName, sep = "."), yError = stringToNum(yErrorVals))
      timeValues$xDimension <- xDim
      timeValues$xUnit <- xUnit
      timeValues$yDimension <- yDim
      timeValues$yUnit <- yUnit
      timeValues$yErrorUnit <- yErrorUnit
      timeValues$dataType <- XYDataTypes$Observed
      timeValues$setMetaData(name = "StudyId", value = group$`Study Id`[[1]])
      timeValues$setMetaData(name = "PatientId", value = group$`Study Id`[[1]])
      timeValues$setMetaData(name = "Organ", value = group$Organ[[1]])
      timeValues$setMetaData(name = "Compartment", value = group$Compartment[[1]])
      timeValues$setMetaData(name = "Species", value = group$Species[[1]])
      timeValues$setMetaData(name = "Gender", value = group$Gender[[1]])
      timeValues$setMetaData(name = "Molecule", value = group$Molecule[[1]])

      # If a molecule is specified, retrieve its molecular weight
      moleculeName <- timeValues$getAllMetaData()$Molecule
      if (!is.null(moleculeName) && !is.na(moleculeName)) {
        compoundProperties <- readExcel(
          path = file.path(dataConfiguration$dataFolder, dataConfiguration$compoundPropertiesFile), sheet = timeValues$getAllMetaData()$Molecule
        )
        mwIdx <- which(compoundProperties$`Parameter, [AdditionalParameter]` == "MW")
        mw <- compoundProperties$`Value [1,1]`[[mwIdx]]
        unit <- compoundProperties$`Unit [1,1]`[[mwIdx]]
        timeValues$MW <- as.numeric(mw)
      }

      timeValues$setMetaData(name = "GroupId", value = group$`Group Id`[[1]])

      # Some ugly piece of code to create a tree-like structure.
      # Don't even want to comment
      levelString <- unlist(lapply(groupings, function(x) {
        group[[x]][[1]]
      }), use.names = FALSE)
      levelString <- paste0("'", levelString, "'", collapse = "$")
      evalString <- paste0("observedData[[sheet]]$", levelString, " <- timeValues")
      eval(parse(text = evalString))
    }
  }
  return(observedData)
}
