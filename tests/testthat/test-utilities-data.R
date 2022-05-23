## context("utilities-data")

test_that("It can read a properly defined file", {
  dataConf <- DataConfiguration$new(
    dataFolder = getTestDataFilePath(""),
    dataFile = "CompiledDataSet.xlsx",
    compoundPropertiesFile = "Compound_Properties.xlsx",
    dataSheets = c("TestSheet_1")
  )

  observedData <- readOSPSTimeValues(dataConfiguration = dataConf)
  expect_equal(length(observedData[[1]]), 2)
})
