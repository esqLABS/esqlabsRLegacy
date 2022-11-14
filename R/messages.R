messages <- esqlabsR:::messages

messages$errorFileNotFound <- function(filePath, optionalMessage = NULL) {
  paste0("File '", filePath, "' could not be found!")
}

messages$errorEsqlabsRSettingNotFound <- function(settingName) {
  paste0("No global setting with the name '", settingName, "' exists. Available global settings are:\n", paste0(names(esqlabsLegacyEnv), collapse = ", "))
}

messages$errorOutputPathNotFound <- function(string) {
  paste0("The output with the path '", string, "' is not found.")
}

messages$warningLabelNotInDataMapping <- function(string) {
  paste0("No xy-series with label ", string, " exists in the DataMapping. Nothing to remove")
}

messages$errorValuesAreNotPositive <- function(values, optionalMessage = NULL) {
  paste("All values must be positive or 0, but they are not! Values are: ", paste(as.character(values), collapse = ", "), optionalMessage)
}

messages$errorWrongLength <- function(object, length, optionalMessage = NULL) {
  paste("Object `", object, "` must be of length ", length, " but it is not!")
}

messages$errorMultipleMetaDataEntries <- function(optionalMessage = NULL) {
  paste("Can only set a single meta data entry at once", optionalMessage)
}

messages$dataMappingNoGrouping <- function(optionalMessage = NULL) {
  paste("No data sets are grouped in the data mapping!", optionalMessage)
}
