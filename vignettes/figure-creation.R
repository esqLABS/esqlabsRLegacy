## ---- include = FALSE, warning = FALSE----------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  fig.width = 8, fig.height = 8
)

## ---- echo = FALSE, results = "hide", message = F-----------------------------
library(esqlabsRLegacy)

## ----createDataMapping--------------------------------------------------------
dataMapping <- DataMapping$new()
print(dataMapping)

## ----loadData-----------------------------------------------------------------
dataConfiguration <- DataConfiguration$new(
  dataFolder = file.path(getwd(), "..", "tests", "data"),
  dataFile = "CompiledDataSet.xlsx",
  compoundPropertiesFile = "Compound_Properties.xlsx",
  dataSheets = c("Stevens_2012_placebo")
)
observedData <- readOSPSTimeValues(dataConfiguration)

## ----addObsDataToMapping------------------------------------------------------
dataMapping <- DataMapping$new()

dataMapping$addXYData(
  list(
    observedData$Stevens_2012_placebo$Placebo_distal,
    observedData$Stevens_2012_placebo$Placebo_proximal,
    observedData$Stevens_2012_placebo$Placebo_total
  ),
  groups = list(
    "Stevens 2012 solid distal",
    "Stevens 2012 solid proximal",
    "Stevens 2012 solid total"
  )
)

dataMapping$plot()

## ----changeAxisAndTitle-------------------------------------------------------
dataMapping$xLab <- "Time [min]"
dataMapping$yLab <- "Gastric retention [%]"
dataMapping$title <- "Stevens 2012"

dataMapping$plot()

## ----addSimResultsToMapping---------------------------------------------------
sim <- loadSimulation(file.path(getwd(), "..", "tests", "data", "Stevens_2012_placebo_indiv_results.pkml"))
simResults <- importResultsFromCSV(simulation = sim, filePaths = file.path(getwd(), "..", "tests", "data", "Stevens_2012_placebo_indiv_results.csv"))

dataMapping$addModelOutputs(
  paths = list(
    "Organism|Lumen|Stomach|Metformin|Gastric retention",
    "Organism|Lumen|Stomach|Metformin|Gastric retention distal",
    "Organism|Lumen|Stomach|Metformin|Gastric retention proximal"
  ),
  simulationResults = simResults,
  list(
    "Stevens_2012_placebo solid total sim",
    "Stevens_2012_placebo solid distal sim",
    "Stevens_2012_placebo solid proximal sim"
  )
)

dataMapping$plot()

## ----addSimResultsToMappingAndGrouping----------------------------------------
dataMapping$addModelOutputs(
  paths = list(
    "Organism|Lumen|Stomach|Metformin|Gastric retention",
    "Organism|Lumen|Stomach|Metformin|Gastric retention distal",
    "Organism|Lumen|Stomach|Metformin|Gastric retention proximal"
  ),
  simulationResults = simResults,
  list(
    "Stevens_2012_placebo solid total sim",
    "Stevens_2012_placebo solid distal sim",
    "Stevens_2012_placebo solid proximal sim"
  ),
  groups = list(
    "Stevens 2012 solid total",
    "Stevens 2012 solid distal",
    "Stevens 2012 solid proximal"
  )
)

dataMapping$plot()

## ----changeAxisUnits----------------------------------------------------------
dataMapping$xUnit <- "h"
dataMapping$yUnit <- "%"
dataMapping$xLab <- paste(dataMapping$xDimension, "[", dataMapping$xUnit, "]")

dataMapping$plot()

## ----predVsObserved-----------------------------------------------------------
dataMappingPvO <- dataMapping$clone(deep = TRUE)
dataMappingPvO$log <- ""
dataMappingPvO$plotType <- PlotTypes$PredictedVsObserved
dataMappingPvO$plot()

## ----predVsObservedLogY-------------------------------------------------------
dataMappingPvO$log <- "y"
dataMappingPvO$plot(foldDistance = c(2, 5))

## ----boxPlot------------------------------------------------------------------
dataMappingBoxPlot <- dataMapping$clone(deep = TRUE)
dataMappingBoxPlot$plotType <- PlotTypes$BoxPlot
dataMappingBoxPlot$plot()

## ----changeAxisLimits---------------------------------------------------------
dataMapping$xLim <- c(-2, 6)
dataMapping$yLim <- c(-30, 150)

dataMapping$plot()

## ----setConfiguration---------------------------------------------------------
labels <- names(dataMapping$xySeries)

configuration <- DataMappingConfiguration$new()
configuration$setXFactors(labels = c(labels[[1]], labels[[2]]), xFactors = c(2, 0))
configuration$setYFactors(labels = c(labels[[1]], labels[[2]]), yFactors = c(3, 2))
configuration$setXOffsets(labels = c(labels[[1]], labels[[2]]), xOffsets = c(3, 2))
configuration$setYOffsets(labels = c(labels[[1]], labels[[2]]), yOffsets = c(3, 2))

dataMapping$setConfiguration(dataMappingConfiguration = configuration)
dataMapping$plot()

## ----setYAxisToLog------------------------------------------------------------
dataMapping$log <- "y"
dataMapping$plot()

## ----loadAndPlotPopSim--------------------------------------------------------
dataMappingPop <- DataMapping$new()

dataMappingPop$addXYData(
  list(
    observedData$Stevens_2012_placebo$Placebo_distal,
    observedData$Stevens_2012_placebo$Placebo_proximal,
    observedData$Stevens_2012_placebo$Placebo_total
  ),
  groups = list(
    "Stevens 2012 solid distal",
    "Stevens 2012 solid proximal",
    "Stevens 2012 solid total"
  )
)

sim <- loadSimulation(file.path(getwd(), "..", "tests", "data", "Stevens_2012_placebo_indiv_results.pkml"))
popResults <- importResultsFromCSV(simulation = sim, filePaths = file.path(getwd(), "..", "tests", "data", "Stevens_2012_placebo_pop_results.csv"))

dataMappingPop$addModelOutputs(
  paths = list(
    "Organism|Lumen|Stomach|Metformin|Gastric retention",
    "Organism|Lumen|Stomach|Metformin|Gastric retention distal",
    "Organism|Lumen|Stomach|Metformin|Gastric retention proximal"
  ),
  simulationResults = popResults,
  labels = list(
    "Stevens_2012_placebo solid total sim",
    "Stevens_2012_placebo solid distal sim",
    "Stevens_2012_placebo solid proximal sim"
  ),
  groups = list(
    "Stevens 2012 solid total",
    "Stevens 2012 solid distal",
    "Stevens 2012 solid proximal"
  )
)

dataMappingPop$plotType <- PlotTypes$PopulationQuantiles

dataMappingPop$plot()

## ----createPlotConfiguration--------------------------------------------------
plotConfiguration <- PlotConfiguration$new()
plotConfiguration$outputName <- "Gastric emptying solid meal"
plotConfiguration$addTitle <- TRUE

## ----plotMultiPanel, fig.width = 12, fig.height = 12--------------------------
plotMultiPanel(
  dataMappingList = list(dataMapping, dataMappingPvO, dataMappingBoxPlot, dataMappingPop),
  plotConfiguration = plotConfiguration
)

