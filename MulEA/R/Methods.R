calculateHypergeometricTest <- function(model, sampleVector, poolVector = NULL) {
  sampleVector <- checkIfPoolIncludeSample(model, sampleVector, poolVector)

  if (0 != length(poolVector)) {
    allElements <- unique(poolVector)
    model <- cutGmtToPool(gmt = model, pool = poolVector)
  } else {
    allElements <- unique(unlist(model$listOfValues))
  }

  testResults <- ddply(.data = model,  .variables = c("ontologyId"), .fun = function(dfRow) {
    poolAndSelectedAndDBiIntersection <- intersect(sampleVector, dfRow[1, 'listOfValues'][[1]])

    selectedAndInGroup <- length(poolAndSelectedAndDBiIntersection)
    selectedAndOutOfGroup <- length(setdiff(sampleVector, poolAndSelectedAndDBiIntersection))
    outOfSelectionAndInGroup <- length(setdiff(dfRow[1, 'listOfValues'][[1]], sampleVector))
    outOfSelectionAndOutOfGroup <- length(setdiff(allElements, union(sampleVector, dfRow[1, 'listOfValues'][[1]])))

    contingencyTable <- matrix(c(selectedAndInGroup,
                                 selectedAndOutOfGroup,
                                 outOfSelectionAndInGroup,
                                 outOfSelectionAndOutOfGroup),
                               2, 2)

    data <- data.frame(
      'ontologyName' = dfRow['ontologyName'],
      'listOfValues' = dfRow["listOfValues"],
      'overlappingData' = I(list(poolAndSelectedAndDBiIntersection)),
      'contingencyTable' = I(list(contingencyTable)),
      'p.value' = phyper(selectedAndInGroup,
                         selectedAndInGroup + outOfSelectionAndInGroup,
                         selectedAndOutOfGroup + outOfSelectionAndOutOfGroup,
                         selectedAndInGroup + selectedAndOutOfGroup, lower.tail = TRUE))
  })
  testResults
}

checkIfPoolIncludeSample <- function(model, sampleVector, poolVector = NULL) {
  # Chcking if experiment data all in model data.
  if (0 != length(poolVector)) {
    if (0 != sum(!(sampleVector %in% poolVector))) {
      warning("testData are outside of pool.", " ",
              paste(setdiff(sampleVector, unique(poolVector)), collapse = ", "))
      return(setdiff(sampleVector, setdiff(sampleVector, unique(poolVector))))
    }
  } else {
    if (0 != sum(!(sampleVector %in% unique(unlist(model$listOfValues))))) {
      warning("testData are outside of gmt.", " ",
              paste(setdiff(sampleVector, unique(unlist(model$listOfValues))), collapse = ", "))
      return(setdiff(sampleVector, setdiff(sampleVector, unique(unlist(model$listOfValues)))))
    }
  }
  return(sampleVector)
}

cutGmtToPool <- function(gmt, pool) {
  cutDF <- plyr::ddply(.data = gmt,  .variables = c("ontologyId"), .fun = function(dfRow) {
    dfRow$listOfValues[[1]] <- intersect(dfRow$listOfValues[[1]], pool)
    dfRow
  })
  cutDF
}

gseaPermutationTest <- function(modelWithTestDf, steps, sampleVector, poolVector = character(0)) {
  sampleVector <- checkIfPoolIncludeSample(modelWithTestDf, sampleVector, poolVector)

  R_value_obs <- integer(0)
  for (i in 1:length(modelWithTestDf$p.value)) {
    # R_value_obs[i] <- sum(modelWithTestDf$p.value <= modelWithTestDf$p.value[i])
    R_value_obs[i] <- sum(modelWithTestDf$p.value[i] <= modelWithTestDf$p.value)
  }

  print(R_value_obs)

  if (0 != length(poolVector)) {
    allElements <- unique(poolVector)
    modelWithTestDf <- cutGmtToPool(gmt = modelWithTestDf, pool = poolVector)
  } else {
    allElements <- unique(unlist(modelWithTestDf$listOfValues))
  }

  simulationMatrix <- array(dim = c(length(modelWithTestDf$p.value), steps))
  print(simulationMatrix)
  for (j in 1:steps) {
    randomData <- sample(allElements, length(sampleVector))
    for (i in 1:length(modelWithTestDf$p.value)) {
      poolAndSelectedAndDBiIntersection <- intersect(randomData, modelWithTestDf[i, 'listOfValues'][[1]])

      selectedAndInGroup <- length(poolAndSelectedAndDBiIntersection)
      selectedAndOutOfGroup <- length(randomData) - selectedAndInGroup
      outOfSelectionAndInGroup <- length(modelWithTestDf[i, 'listOfValues'][[1]]) - selectedAndInGroup
      outOfSelectionAndOutOfGroup <- length(allElements) - (selectedAndInGroup + selectedAndOutOfGroup + outOfSelectionAndInGroup)

      simulationMatrix[i,j] = phyper(selectedAndInGroup,
                                     selectedAndInGroup + outOfSelectionAndInGroup,
                                     selectedAndOutOfGroup + outOfSelectionAndOutOfGroup,
                                     selectedAndInGroup + selectedAndOutOfGroup, lower.tail = TRUE)
    }
  }


  print(simulationMatrix)
  simulationVector <- as.vector(simulationMatrix)
  print(simulationVector)

  R_value_exp <- integer(0)
  for (l in 1:length(modelWithTestDf$p.value)) {
    # Why is 15 digits?
    # P_Sim_round=round(P_Sim_vec, digits=15)
    # R_value_exp[l] <- sum(simulationVector <= modelWithTestDf$p.value[l]) / steps
    R_value_exp[l] <- sum(modelWithTestDf$p.value[l] <= simulationVector) / steps
    # R_value_exp[l] <- sum(as.vector(simulationMatrix[l,]) <= modelWithTestDf$p.value[l]) / steps
  }

  print(R_value_exp)

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
  for (i in 1:length(modelWithTestDf$ontologyId)) {
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
    adjustResult <- data.frame(modelWithTestsResults, "q.value" = gseaPermutationTest(modelWithTestDf = modelWithTestsResults, steps = steps, sampleVector = sampleVector, poolVector = poolVector))
  } else {
    adjustResult <- data.frame(modelWithTestsResults, "q.value" = p.adjust(modelWithTestsResults$p.value, method = adjustMethod))
  }
  adjustResult
}


calculateTestOnContingencyTable <- function(testMethod, ...) {
  function(model, sampleVector, poolVector = NULL) {
    sampleVector <- checkIfPoolIncludeSample(model, sampleVector, poolVector)


    if (0 != length(poolVector)) {
      allElements <- unique(poolVector)
      model <- cutGmtToPool(gmt = model, pool = poolVector)
    } else {
      allElements <- unique(unlist(model$listOfValues))
    }

    testResults <- ddply(.data = model,  .variables = c("ontologyId"), .fun = function(dfRow) {
      poolAndSelectedAndDBiIntersection <- intersect(sampleVector, dfRow[1, 'listOfValues'][[1]])

      selectedAndInGroup <- length(poolAndSelectedAndDBiIntersection)
      selectedAndOutOfGroup <- length(setdiff(sampleVector, poolAndSelectedAndDBiIntersection))
      outOfSelectionAndInGroup <- length(setdiff(dfRow[1, 'listOfValues'][[1]], sampleVector))
      outOfSelectionAndOutOfGroup <- length(setdiff(allElements, union(sampleVector, dfRow[1, 'listOfValues'][[1]])))

      contingencyTable <- matrix(c(selectedAndInGroup,
                                   selectedAndOutOfGroup,
                                   outOfSelectionAndInGroup,
                                   outOfSelectionAndOutOfGroup),
                                 2, 2)
      data <- data.frame(
        'ontologyName' = dfRow['ontologyName'],
        'listOfValues' = dfRow["listOfValues"],
        'overlappingData' = I(list(poolAndSelectedAndDBiIntersection)),
        'contingencyTable' = I(list(contingencyTable)),
        'p.value' = testMethod(contingencyTable, ...)$p.value)
    })
    testResults
  }
}

calculateFisherTest <- calculateTestOnContingencyTable(fisher.test, alternative = 'less')

calculateChiSquaredTest <- calculateTestOnContingencyTable(chisq.test)
