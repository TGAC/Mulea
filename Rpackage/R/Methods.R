calculateHypergeometricTest <- function(model, sampleVector) {
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
      modelSampleIntersection <- intersect(modelWithTestDf[i, 'listOfValues'][[1]], randomData)
      q <- length(modelSampleIntersection)
      m <- length(modelWithTestDf[i, 'listOfValues'][[1]])
      n <- length(setdiff(allElements, modelWithTestDf[i, 'listOfValues'][[1]]))
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
  gseaPermutationTestVector <- R_value_exp / R_value_obs
  gseaPermutationTestVector
}

gseaPermutationTestPlyr <- function(modelWithTestDf, steps, sampleVector) {
  R_value_obs <- plyr::daply(.data = modelWithTestDf, .variables = c("category"), .fun = function(dfRow) {
    sum(modelWithTestDf$p.value <= dfRow$p.value)
  })

  allElements <- unique(unlist(modelWithTestDf$listOfValues))
  k <- 1
  w <- nrow(modelWithTestDf)
  simulationVector <- numeric(steps * w)
  for (j in 1:steps) {
    randomData <- sample(allElements, length(sampleVector))
    simulationVector[k:(k - 1 + w)] <- plyr::daply(.data = modelWithTestDf, .variables = c("category"), .fun = function(dfRow) {
      modelSampleIntersection <- intersect(dfRow$listOfValues[[1]], randomData)
      q <- length(modelSampleIntersection)
      m <- length(dfRow$listOfValues[[1]])
      n <- length(setdiff(allElements, dfRow$listOfValues[[1]]))
      k <- length(randomData)

      # Why 1 - pvalue from test?
      1 - phyper(q, m, n, k, lower.tail = FALSE, log.p = FALSE)
    })
    k <- k + w
  }

  R_value_exp <- plyr::daply(.data = modelWithTestDf, .variables = c("category"), .fun = function(dfRow) {
    sum(simulationVector <= dfRow$p.value)
  })

  gseaPermutationTestVector <- R_value_exp / R_value_obs
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


calculateTestOnContingencyTable <- function(testMethod) {
  function(model, sampleVector) {
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
    testResults
  }
}

calculateFisherTest <- calculateTestOnContingencyTable(fisher.test)

calculateChiSquaredTest <- calculateTestOnContingencyTable(chisq.test)
