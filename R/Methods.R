calculateHypergeometricTest <- function(model, sampleVector, adjustMethod = NA) {
    allElements <- unique(unlist(model$listOfValues))
    testResults <- ddply(.data = model,  .variables = c("category"), .fun = function(dfRow) {
        modelSampleIntersection <- intersect(dfRow[1, 'listOfValues'][[1]], sampleVector)
        q <- length(modelSampleIntersection)
        m <- length(dfRow[1, 'listOfValues'][[1]])
        n <- length(setdiff(allElements, dfRow[1, 'listOfValues'][[1]]))
        k <- length(sampleVector)
        data <- data.frame(
            'description' = dfRow['description'],
            'listOfValues' = dfRow["listOfValues"],
            'listOfValuesUnderCategory' = I(list(modelSampleIntersection)),
            'q' = q,
            'm' = m,
            'n' = n,
            'k' = k,
            'p-value' = phyper(q, m, n, k, lower.tail = FALSE, log.p = FALSE))
    })
    if (!is.na(adjustMethod)) {
      testResults <- data.frame(testResults, "controllingProcedures" = p.adjust(testResults$p.value, method = adjustMethod))
    }
    testResults
}

calculateTestOnContingencyTable <- function(testMethod) {
  function(model, sampleVector, adjustMethod = NA) {
    allElements <- unique(unlist(model$listOfValues))
    testResults <- ddply(.data = model,  .variables = c("category"), .fun = function(dfRow) {
      modelSampleIntersection <- intersect(dfRow[1, 'listOfValues'][[1]], sampleVector)
      q <- length(modelSampleIntersection)
      m <- length(dfRow[1, 'listOfValues'][[1]])
      n <- length(setdiff(allElements, dfRow[1, 'listOfValues'][[1]]))
      k <- length(setdiff(sampleVector, modelSampleIntersection))
      data <- data.frame(
        'description' = dfRow['description'],
        'listOfValues' = dfRow["listOfValues"],
        'listOfValuesUnderCategory' = I(list(modelSampleIntersection)),
        'q' = q,
        'm' = m,
        'n' = n,
        'k' = k,
        'testResultsColumnName' = I(list(testMethod(matrix(c(m, q, n, k), nrow = 2)))))
    })
    if (!is.na(adjustMethod)) {
      testResults <- data.frame(testResults,
                                "controllingProcedures" = p.adjust(
                                    plyr::laply(.data = testResults$testResultsColumnName,
                                                .fun = function(list) {
                                                    list$p.value
                                                }),
                                    method = adjustMethod))
    }
    testResults
  }
}

calculateFisherTest <- calculateTestOnContingencyTable(fisher.test)

calculateChiSquaredTest <- calculateTestOnContingencyTable(chisq.test)

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
