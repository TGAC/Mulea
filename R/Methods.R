calculateHypergeometricTest <- function(model, sampleVector) {
    allElements <- unique(unlist(model$collection))
    ddply(.data = model,  .variables = c("collection_id"), .fun = function(dfRow) {
        data <- data.frame(
            'collection_name' = dfRow['collection_name'],
            'collection' = dfRow["collection"],
            'category collection' = I(list(intersect(dfRow[1, 'collection'][[1]], sampleVector))),
            'q' = length(intersect(dfRow[1, 'collection'][[1]], sampleVector)),
            'm' = length(dfRow[1, 'collection'][[1]]),
            'n' = length(setdiff(allElements, dfRow[1, 'collection'][[1]])),
            'k' = length(sampleVector),
            'p-value' = phyper(length(intersect(dfRow[1, 'collection'][[1]], sampleVector)),
                               length(dfRow[1, 'collection'][[1]]),
                               length(setdiff(allElements, dfRow[1, 'collection'][[1]])),
                               length(sampleVector), lower.tail = FALSE, log.p = FALSE))
    })
}

calculateTestOnContingencyTable <- function(testMethod, testResultsColumnName) {
  function(model, sampleVector) {
    allElements <- unique(unlist(model$collection))
    ddply(.data = model,  .variables = c("collection_id"), .fun = function(dfRow) {
      data <- data.frame(
        'collection_name' = dfRow['collection_name'],
        'collection' = dfRow["collection"],
        'category collection' = I(list(intersect(dfRow[1, 'collection'][[1]], sampleVector))),
        'q' = length(intersect(dfRow[1, 'collection'][[1]], sampleVector)),
        'm' = length(dfRow[1, 'collection'][[1]]),
        'n' = length(setdiff(allElements, dfRow[1, 'collection'][[1]])),
        'k' = length(setdiff(sampleVector, intersect(dfRow[1, 'collection'][[1]], sampleVector))),
        testResultsColumnName = I(list(testMethod(matrix(c(length(dfRow[1, 'collection'][[1]]),
                                                         length(intersect(dfRow[1, 'collection'][[1]], sampleVector)),
                                                         length(setdiff(allElements, dfRow[1, 'collection'][[1]])),
                                                         length(setdiff(sampleVector, intersect(dfRow[1, 'collection'][[1]], sampleVector)))),
                                                       nrow = 2)))))
    })
  }
}

calculateFisherTest <- calculateTestOnContingencyTable(fisher.test, testResultsColumnName = 'fisherTestResults')

calculateChiSquaredTest <- calculateTestOnContingencyTable(chisq.test, testResultsColumnName = 'chiSquaredTestResults')


