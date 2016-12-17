calculateHypergeometricTest <- function(model, sampleVector) {
    allElements <- unique(unlist(model$listOfValues))
    ddply(.data = model,  .variables = c("category"), .fun = function(dfRow) {
        data <- data.frame(
            'description' = dfRow['description'],
            'listOfValues' = dfRow["listOfValues"],
            'category listOfValues' = I(list(intersect(dfRow[1, 'listOfValues'][[1]], sampleVector))),
            'q' = length(intersect(dfRow[1, 'listOfValues'][[1]], sampleVector)),
            'm' = length(dfRow[1, 'listOfValues'][[1]]),
            'n' = length(setdiff(allElements, dfRow[1, 'listOfValues'][[1]])),
            'k' = length(sampleVector),
            'p-value' = phyper(length(intersect(dfRow[1, 'listOfValues'][[1]], sampleVector)),
                               length(dfRow[1, 'listOfValues'][[1]]),
                               length(setdiff(allElements, dfRow[1, 'listOfValues'][[1]])),
                               length(sampleVector), lower.tail = FALSE, log.p = FALSE))
    })
}

calculateTestOnContingencyTable <- function(testMethod, testResultsColumnName) {
  function(model, sampleVector) {
    allElements <- unique(unlist(model$listOfValues))
    ddply(.data = model,  .variables = c("category"), .fun = function(dfRow) {
      data <- data.frame(
        'description' = dfRow['description'],
        'listOfValues' = dfRow["listOfValues"],
        'category listOfValues' = I(list(intersect(dfRow[1, 'listOfValues'][[1]], sampleVector))),
        'q' = length(intersect(dfRow[1, 'listOfValues'][[1]], sampleVector)),
        'm' = length(dfRow[1, 'listOfValues'][[1]]),
        'n' = length(setdiff(allElements, dfRow[1, 'listOfValues'][[1]])),
        'k' = length(setdiff(sampleVector, intersect(dfRow[1, 'listOfValues'][[1]], sampleVector))),
        testResultsColumnName = I(list(testMethod(matrix(c(length(dfRow[1, 'listOfValues'][[1]]),
                                                         length(intersect(dfRow[1, 'listOfValues'][[1]], sampleVector)),
                                                         length(setdiff(allElements, dfRow[1, 'listOfValues'][[1]])),
                                                         length(setdiff(sampleVector, intersect(dfRow[1, 'listOfValues'][[1]], sampleVector)))),
                                                       nrow = 2)))))
    })
  }
}

calculateFisherTest <- calculateTestOnContingencyTable(fisher.test, testResultsColumnName = 'fisherTestResults')

calculateChiSquaredTest <- calculateTestOnContingencyTable(chisq.test, testResultsColumnName = 'chiSquaredTestResults')

calculateGSEATest <- function(model, sampleVector, modelBaseVector) {
    modelTmpGmtFile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".gmt")
    saveModelFromDataFrameToGmtFile(modelDF = model[ , !(names(model) %in% c("collection_name"))], gmtFilePath = modelTmpGmtFile)
    sampleBaseTmpFile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".tmp")
    write(sampleVector, file = sampleBaseTmpFile, ncolumns = 1, append = FALSE)
    allElements <- unique(unlist(model$collection))
    modelBaseVector <- allElements
    modelBaseTmpFile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".tmp")
    write(modelBaseVector, file = modelBaseTmpFile, ncolumns = 1, append = FALSE)
    # TODO : How to present results? Now it is list.
    runGSEA(databaseFilePath = modelTmpGmtFile, sampleFilePath = sampleBaseTmpFile, populationFilePath = modelBaseTmpFile)
}
