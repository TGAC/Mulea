calculateHypergeometricTest <- function(model, sampleVector, poolVector = NULL) {
  # Chcking if experiment data all in model data.
  if (0 != length(poolVector)) {
    if (0 != sum(!(sampleVector %in% poolVector))) {
      warning("testData are outside of pool.", " ",
              paste(setdiff(sampleVector, unique(unlist(model$listOfValues))), collapse = ", "))
      return(NA)
    }
  } else {
    if (0 != sum(!(sampleVector %in% unique(unlist(model$listOfValues))))) {
      warning("testData are outside of gmt.", " ",
              paste(setdiff(sampleVector, unique(unlist(model$listOfValues))), collapse = ", "))
      return(NA)
    }
  }

  if (0 != length(poolVector)) {
    allElements <- unique(poolVector)
    testResults <- ddply(.data = model,  .variables = c("category"), .fun = function(dfRow) {
      poolAndSelectedAndDBiIntersection <- Reduce(intersect, list(allElements, sampleVector, dfRow[1, 'listOfValues'][[1]]))
      modelPoolIntersection <- intersect(dfRow[1, 'listOfValues'][[1]], allElements)

      selectedAndInGroup <- length(poolAndSelectedAndDBiIntersection)
      selectedAndOutOfGroup <- length(setdiff(sampleVector, poolAndSelectedAndDBiIntersection))
      outOfSelectionAndInGroup <- length(setdiff(modelPoolIntersection, sampleVector))
      outOfSelectionAndOutOfGroup <- length(setdiff(allElements, union(sampleVector, dfRow[1, 'listOfValues'][[1]])))

      data <- data.frame(
        'description' = dfRow['description'],
        'listOfValues' = dfRow["listOfValues"],
        'listOfValuesUnderCategory' = I(list(poolAndSelectedAndDBiIntersection)),
        'q' = selectedAndInGroup,
        'm' = selectedAndInGroup + outOfSelectionAndInGroup,
        'n' = selectedAndOutOfGroup + outOfSelectionAndOutOfGroup,
        'k' = selectedAndInGroup + selectedAndOutOfGroup,
        'p-value' = phyper(selectedAndInGroup,
                           selectedAndInGroup + outOfSelectionAndInGroup,
                           selectedAndOutOfGroup + outOfSelectionAndOutOfGroup,
                           selectedAndInGroup + selectedAndOutOfGroup, lower.tail = TRUE))
    })
  } else {
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
  }
  testResults
}


gseaPermutationTest <- function(modelWithTestDf, steps, sampleVector, poolVector = character(0)) {
  R_value_obs <- integer(0)
  for (i in 1:length(modelWithTestDf$p.value)) {
    R_value_obs[i] <- sum(modelWithTestDf$p.value <= modelWithTestDf$p.value[i])
  }

  if (0 != length(poolVector)) {
    if (0 != sum(!(sampleVector %in% poolVector))) {
      warning("Samples are outside of pool.")
    }

    allElements <- unique(poolVector)
    simulationMatrix <- array(dim = c(length(modelWithTestDf$p.value), steps))
    for (j in 1:steps) {
      randomData <- sample(allElements, length(sampleVector))
      for (i in 1:length(modelWithTestDf$p.value)) {


        poolAndSelectedAndDBiIntersection <- Reduce(intersect, list(allElements, randomData, modelWithTestDf[i, 'listOfValues'][[1]]))
        modelPoolIntersection <- intersect(modelWithTestDf[i, 'listOfValues'][[1]], allElements)

        selectedAndInGroup <- length(poolAndSelectedAndDBiIntersection)
        selectedAndOutOfGroup <- length(setdiff(randomData, poolAndSelectedAndDBiIntersection))
        outOfSelectionAndInGroup <- length(setdiff(modelPoolIntersection, randomData))
        outOfSelectionAndOutOfGroup <- length(setdiff(allElements, union(randomData, modelWithTestDf[i, 'listOfValues'][[1]])))


        simulationMatrix[i,j] = phyper(selectedAndInGroup,
                                       selectedAndInGroup + outOfSelectionAndInGroup,
                                       selectedAndOutOfGroup + outOfSelectionAndOutOfGroup,
                                       selectedAndInGroup + selectedAndOutOfGroup, lower.tail = TRUE)

        # q <- sum(duplicated(c(modelWithTestDf[i, 'listOfValues'][[1]], randomData)))
        # m <- sum(duplicated(c(modelWithTestDf[i, 'listOfValues'][[1]], allElements)))
        # n <- length(allElements) - m
        # k <- length(randomData)
      }
    }
  } else {
    allElements <- unique(unlist(modelWithTestDf$listOfValues))
    simulationMatrix <- array(dim = c(length(modelWithTestDf$p.value), steps))
    for (j in 1:steps) {
      randomData <- sample(allElements, length(sampleVector))
      for (i in 1:length(modelWithTestDf$p.value)) {
        modelSampleIntersection <- intersect(modelWithTestDf[i, 'listOfValues'][[1]], randomData)

        selectedAndInGroup <- length(modelSampleIntersection)
        selectedAndOutOfGroup <- length(setdiff(randomData, modelSampleIntersection))
        outOfSelectionAndInGroup <- length(setdiff(modelWithTestDf[i, 'listOfValues'][[1]], randomData))
        outOfSelectionAndOutOfGroup <- length(setdiff(allElements, union(randomData, modelWithTestDf[i, 'listOfValues'][[1]])))

        simulationMatrix[i,j] = phyper(selectedAndInGroup,
                             selectedAndInGroup + outOfSelectionAndInGroup,
                             selectedAndOutOfGroup + outOfSelectionAndOutOfGroup,
                             selectedAndInGroup + selectedAndOutOfGroup, lower.tail = TRUE)
      }
    }
  }

  simulationVector <- as.vector(simulationMatrix)

  R_value_exp <- integer(0)
  for (l in 1:length(modelWithTestDf$p.value)) {
    # Why is 15 digits?
    # P_Sim_round=round(P_Sim_vec, digits=15)
    R_value_exp[l] <- sum(simulationVector <= modelWithTestDf$p.value[l]) / steps
  }
  gseaPermutationTestVector <- round(R_value_exp / R_value_obs, digits = 10)
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


adjustPvaluesForMultipleComparisons <- function(modelWithTestsResults, sampleVector, poolVector = character(0), adjustMethod = "bonferroni", steps = 1) {
  if (adjustMethod == "GSEA") {
    adjustResult <- data.frame(modelWithTestsResults, "controllingProcedures" = gseaPermutationTest(modelWithTestDf = modelWithTestsResults, steps = steps, sampleVector = sampleVector, poolVector = poolVector))
  } else {
    adjustResult <- data.frame(modelWithTestsResults, "controllingProcedures" = p.adjust(modelWithTestsResults$p.value, method = adjustMethod))
  }
  adjustResult
}


calculateTestOnContingencyTable <- function(testMethod, ...) {
  function(model, sampleVector, poolVector = NULL) {
    # Chcking if experiment data all in model data.
    if (0 != length(poolVector)) {
      if (0 != sum(!(sampleVector %in% poolVector))) {
        warning("testData are outside of pool.", " ",
                paste(setdiff(sampleVector, unique(unlist(model$listOfValues))), collapse = ", "))
        return(NA)
      }
    } else {
      if (0 != sum(!(sampleVector %in% unique(unlist(model$listOfValues))))) {
        warning("testData are outside of gmt.", " ",
                paste(setdiff(sampleVector, unique(unlist(model$listOfValues))), collapse = ", "))
        return(NA)
      }
    }

    if (0 != length(poolVector)) {
      allElements <- unique(poolVector)
    } else {
      allElements <- unique(unlist(model$listOfValues))
    }
    testResults <- ddply(.data = model,  .variables = c("category"), .fun = function(dfRow) {

      if (0 != length(poolVector)) {
        listOfValuesUnderCategory <- modelSampleIntersection <- intersect(dfRow[1, 'listOfValues'][[1]], sampleVector)
        selectedAndInGroup <- length(modelSampleIntersection)
        selectedAndOutOfGroup <- length(setdiff(sampleVector, modelSampleIntersection))
        outOfSelectionAndInGroup <- length(setdiff(dfRow[1, 'listOfValues'][[1]], sampleVector))
        outOfSelectionAndOutOfGroup <- length(setdiff(allElements, union(sampleVector, dfRow[1, 'listOfValues'][[1]])))
      } else {
        listOfValuesUnderCategory <- poolAndSelectedAndDBiIntersection <- Reduce(intersect, list(allElements, sampleVector, dfRow[1, 'listOfValues'][[1]]))
        modelPoolIntersection <- intersect(dfRow[1, 'listOfValues'][[1]], allElements)

        selectedAndInGroup <- length(poolAndSelectedAndDBiIntersection)
        selectedAndOutOfGroup <- length(setdiff(sampleVector, poolAndSelectedAndDBiIntersection))
        outOfSelectionAndInGroup <- length(setdiff(modelPoolIntersection, sampleVector))
        outOfSelectionAndOutOfGroup <- length(setdiff(allElements, union(sampleVector, dfRow[1, 'listOfValues'][[1]])))
      }

      contingencyTable <- matrix(c(selectedAndInGroup,
                                   selectedAndOutOfGroup,
                                   outOfSelectionAndInGroup,
                                   outOfSelectionAndOutOfGroup),
                                 2, 2)
      data <- data.frame(
        'description' = dfRow['description'],
        'listOfValues' = dfRow["listOfValues"],
        'listOfValuesUnderCategory' = I(list(listOfValuesUnderCategory)),
        'contingencyTable' = I(list(contingencyTable)),
        'testResultsColumnName' = I(list(testMethod(contingencyTable, ...))))
    })
    testResults
  }
}

calculateFisherTest <- calculateTestOnContingencyTable(fisher.test, alternative = 'less')

calculateChiSquaredTest <- calculateTestOnContingencyTable(chisq.test)
