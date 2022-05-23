## context("DataConfiguration")

test_that("It can create a instance of DataConfiguration", {
  dataConf <- DataConfiguration$new(
    dataFolder = getTestDataFilePath(""),
    dataFile = "CompiledDataSet.xlsx",
    compoundPropertiesFile = "Compound_Properties.xlsx",
    dataSheets = c("TestSheet_1")
  )

  expect_equal(dataConf$dataFolder, getTestDataFilePath(""))
  expect_equal(dataConf$dataFile, "CompiledDataSet.xlsx")
  expect_equal(dataConf$compoundPropertiesFile, "Compound_Properties.xlsx")
  expect_equal(dataConf$dataSheets, "TestSheet_1")
  expect_equal(dataConf$columnsToSplitBy, esqlabsLegacyEnv$columnsToSplitDataBy)
  expect_equal(dataConf$XValuesColumn, esqlabsLegacyEnv$XValuesColumn)
  expect_equal(dataConf$YValuesColumn, esqlabsLegacyEnv$YValuesColumn)
  expect_equal(dataConf$YErrorColumn, esqlabsLegacyEnv$YErrorColumn)

  expect_error(capture.output(print(dataConf)), NA)
})
