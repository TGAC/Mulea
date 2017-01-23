modelDfFromFile <- MulEA::readGmtFileAsDF(gmtFilePath = "/home/koralgooll/doktorat/Rpackages/Mulea/example/GO2allGenes_FBgnIDs_v2.gmt")
steps <- 2
sampleVector <- sample(unique(unlist(modelDfFromFile$listOfValues)), 200)
modelWithTestDf <- MulEA::calculateHypergeometricTest(model = modelDfFromFile, sampleVector = sampleVector)
length(unique(unlist(modelWithTestDf$listOfValues)))


a2X <- function() {
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
        simulationMatrix[i,j] <- 1 - phyper(q, m, n, k, lower.tail = FALSE, log.p = FALSE)
    }
  }
  simulationVector <- as.vector(simulationMatrix)
  simulationVector
}


a2Y <- function() {
  allElements <- unique(unlist(modelWithTestDf$listOfValues))
  simulationMatrix <- array(dim = c(length(modelWithTestDf$p.value), steps))
  for (j in 1:steps) {
    randomData <- sample(allElements, length(sampleVector))
    for (i in 1:length(modelWithTestDf$p.value)) {
      # modelSampleIntersection <- intersect(modelWithTestDf[i, 'listOfValues'][[1]], randomData)
      concatenatedListOfValues <- c(modelWithTestDf[i, 'listOfValues'][[1]], randomData)
      modelSampleIntersection <- concatenatedListOfValues[duplicated(concatenatedListOfValues)]
      # modelSampleIntersectionLength <- sum(duplicated(c(modelWithTestDf[i, 'listOfValues'][[1]], randomData)))
      q <- length(modelSampleIntersection)
      m <- length(modelWithTestDf[i, 'listOfValues'][[1]])
      modelAllElementsIntersectionLength <- sum(duplicated(c(modelWithTestDf[i, 'listOfValues'][[1]], allElements)))
      n <- length(allElements) - modelAllElementsIntersectionLength
      k <- length(randomData)
      # # Why 1 - pvalue from test?
      simulationMatrix[i,j] <- 1 - phyper(q, m, n, k, lower.tail = FALSE, log.p = FALSE)
    }
  }
  simulationVector <- as.vector(simulationMatrix)
  simulationVector
}


b2 <- function() {
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
  simulationVector
}


for (i in c(10, 100, 200)) {
  for (j in c(20, 200, 500)) {
    steps <- i
    sampleVector <- sample(unique(unlist(modelDfFromFile$listOfValues)), j)
    system.time(a2Xdata <- a2X())
    system.time(a2Ydata <- a2Y())
  }
}
system.time(a2Xdata <- a2X())
system.time(a2Ydata <- a2Y())
sum(a2Xdata == a2Ydata)
