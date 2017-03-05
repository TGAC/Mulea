calculateHypergeometricTest <- function(model, sampleVector) {
    allElements <- unique(unlist(model$listOfValues))
    testResults <- ddply(.data = model,  .variables = c("category"), .fun = function(dfRow) {
        modelSampleIntersection <- intersect(dfRow[1, 'listOfValues'][[1]], sampleVector)
        selectedAndInGroup <- length(modelSampleIntersection)
        selectedAndOutOfGroup <- length(setdiff(sampleVector, modelSampleIntersection))
        outOfSelectionAndInGroup <- length(setdiff(dfRow[1, 'listOfValues'][[1]], sampleVector))
        outOfSelectionAndOutOfGroup <- length(setdiff(allElements, union(sampleVector, dfRow[1, 'listOfValues'][[1]])))
        data <- data.frame(
            'description' = dfRow['description'],
            'listOfValues' = dfRow["listOfValues"],
            'listOfValuesUnderCategory' = I(list(modelSampleIntersection)),
            'q' = selectedAndInGroup,
            'm' = selectedAndInGroup + outOfSelectionAndInGroup,
            'n' = selectedAndOutOfGroup + outOfSelectionAndOutOfGroup,
            'k' = selectedAndInGroup + selectedAndOutOfGroup,
            'p-value' = phyper(selectedAndInGroup,
                               selectedAndInGroup + outOfSelectionAndInGroup,
                               selectedAndOutOfGroup + outOfSelectionAndOutOfGroup,
                               selectedAndInGroup + selectedAndOutOfGroup, lower.tail = TRUE))
    })
    testResults
}


gseaPermutationTest <- function(modelWithTestDf, steps, sampleVector) {
  R_value_obs <- integer(0)
  for (i in 1:length(modelWithTestDf$p.value)) {
    R_value_obs[i] <- sum(modelWithTestDf$p.value <= modelWithTestDf$p.value[i])
  }

  allElements <- unique(unlist(modelWithTestDf$listOfValues))
  simulationMatrix <- array(dim = c(length(modelWithTestDf$p.value), steps))
  for (j in 1:steps) {
    randomData <- sample(allElements, length(sampleVector))
    for (i in 1:length(modelWithTestDf$p.value)) {
      q <- sum(duplicated(c(modelWithTestDf[i, 'listOfValues'][[1]], randomData)))
      m <- length(modelWithTestDf[i, 'listOfValues'][[1]])
      n <- length(allElements) - m
      k <- length(randomData)
      # Why 1 - pvalue from test?
      simulationMatrix[i,j] = 1 - phyper(q, m, n, k, lower.tail = FALSE, log.p = FALSE)
    }
  }
  simulationVector <- as.vector(simulationMatrix)

  R_value_exp <- integer(0)
  for (l in 1:length(modelWithTestDf$p.value)) {
    # Why is 15 digits?
    # P_Sim_round=round(P_Sim_vec, digits=15)
    R_value_exp[l] <- sum(simulationVector <= modelWithTestDf$p.value[l]) / steps
  }
  gseaPermutationTestVector <- round(R_value_exp / R_value_obs, digits = 4)
  gseaPermutationTestVector
}

gseaPermutationTestWithBinaryMatrix <- function(modelWithTestDf, steps, sampleVector) {
  R_value_obs <- integer(0)
  for (i in 1:length(modelWithTestDf$p.value)) {
    R_value_obs[i] <- sum(modelWithTestDf$p.value <= modelWithTestDf$p.value[i])
  }

  allElements <- unique(unlist(modelWithTestDf$listOfValues))
  dataMatrix <- list()
  for (i in 1:length(modelWithTestDf$category)) {
    concatenation <- c(modelWithTestDf[i,]$listOfValues[[1]], allElements)
    dataMatrix <- append(dataMatrix, list(bit::as.bit(duplicated(concatenation)[(length(modelWithTestDf[i,]$listOfValues[[1]])+1):length(concatenation)])))
  }

  simulationMatrix <- array(dim = c(length(modelWithTestDf$p.value), steps))
  for (j in 1:steps) {
    randomData <- bit::as.bit.which(sample(length(allElements), size = length(sampleVector)), length(allElements))
    for (i in 1:length(dataMatrix)) {
      q <- sum(dataMatrix[[i]] & randomData)
      m <- sum(dataMatrix[[i]])
      n <- length(allElements) - m
      k <- sum(randomData)
      # Why 1 - pvalue from test?
      simulationMatrix[i,j] <- 1 - phyper(q, m, n, k, lower.tail = FALSE, log.p = FALSE)
    }
  }
  simulationVector <- as.vector(simulationMatrix)

  R_value_exp <- integer(0)
  for (l in 1:length(modelWithTestDf$p.value)) {
    # Why is 15 digits?
    # P_Sim_round=round(P_Sim_vec, digits=15)
    R_value_exp[l] <- sum(simulationVector <= modelWithTestDf$p.value[l]) / steps
  }
  gseaPermutationTestVector <- round(R_value_exp / R_value_obs, digits = 4)
  gseaPermutationTestVector
}

gseaPermutationTestPlyr <- function(modelWithTestDf, steps, sampleVector) {
  R_value_obs <- plyr::daply(.data = modelWithTestDf, .variables = c("category"), .fun = function(dfRow) {
    sum(modelWithTestDf$p.value <= dfRow$p.value)
  })
  names(R_value_obs) <- NULL

  allElements <- unique(unlist(modelWithTestDf$listOfValues))
  k <- 1
  w <- nrow(modelWithTestDf)
  simulationVector <- numeric(steps * w)
  for (j in 1:steps) {
    randomData <- sample(allElements, length(sampleVector))
    simulationVector[k:(k - 1 + w)] <- plyr::daply(.data = modelWithTestDf, .variables = c("category"), .fun = function(dfRow) {
      q <- sum(duplicated(c(dfRow$listOfValues[[1]], randomData)))
      m <- length(dfRow$listOfValues[[1]])
      n <- length(allElements) - m
      k <- length(randomData)
      # Why 1 - pvalue from test?
      1 - phyper(q, m, n, k, lower.tail = FALSE, log.p = FALSE)
    })
    k <- k + w
  }

  R_value_exp <- plyr::daply(.data = modelWithTestDf, .variables = c("category"), .fun = function(dfRow) {
    sum(simulationVector <= dfRow$p.value) / steps
  })
  names(R_value_exp) <- NULL

  gseaPermutationTestVector <- round(R_value_exp / R_value_obs, digits = 4)
  gseaPermutationTestVector
}


adjustPvaluesForMultipleComparisons <- function(modelWithTestsResults, sampleVector, adjustMethod = "bonferroni", steps = 1) {
  if (adjustMethod == "GSEA") {
    adjustResult <- data.frame(modelWithTestsResults, "controllingProcedures" = gseaPermutationTest(modelWithTestDf = modelWithTestsResults, steps = steps, sampleVector = sampleVector))
  } else {
    adjustResult <- data.frame(modelWithTestsResults, "controllingProcedures" = p.adjust(modelWithTestsResults$p.value, method = adjustMethod))
  }
  adjustResult
}


calculateTestOnContingencyTable <- function(testMethod, ...) {
  function(model, sampleVector) {
    allElements <- unique(unlist(model$listOfValues))
    testResults <- ddply(.data = model,  .variables = c("category"), .fun = function(dfRow) {
      modelSampleIntersection <- intersect(dfRow[1, 'listOfValues'][[1]], sampleVector)
      selectedAndInGroup <- length(modelSampleIntersection)
      selectedAndOutOfGroup <- length(setdiff(sampleVector, modelSampleIntersection))
      outOfSelectionAndInGroup <- length(setdiff(dfRow[1, 'listOfValues'][[1]], sampleVector))
      outOfSelectionAndOutOfGroup <- length(setdiff(allElements, union(sampleVector, dfRow[1, 'listOfValues'][[1]])))
      contingencyTable <- matrix(c(selectedAndInGroup,
                                   selectedAndOutOfGroup,
                                   outOfSelectionAndInGroup,
                                   outOfSelectionAndOutOfGroup),
                                 2, 2)
      data <- data.frame(
        'description' = dfRow['description'],
        'listOfValues' = dfRow["listOfValues"],
        'listOfValuesUnderCategory' = I(list(modelSampleIntersection)),
        'contingencyTable' = I(list(contingencyTable)),
        'testResultsColumnName' = I(list(testMethod(contingencyTable, ...))))
    })
    testResults
  }
}

calculateFisherTest <- calculateTestOnContingencyTable(fisher.test, alternative = 'less')

calculateChiSquaredTest <- calculateTestOnContingencyTable(chisq.test)
