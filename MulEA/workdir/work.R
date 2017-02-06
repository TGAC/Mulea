cat("\014")
modelDfFromFile <- MulEA::readGmtFileAsDF(gmtFilePath = "/home/koralgooll/doktorat/Rpackages/Mulea/Rpackage/example/GO2allGenes_FBgnIDs_v2.gmt")
modelDfFromFile <- MulEA::readGmtFileAsDF(gmtFilePath = "/home/koralgooll/doktorat/Rpackages/Mulea/Rpackage/inst/example/model.gmt")
steps <- 2
sampleVector <- sample(unique(unlist(modelDfFromFile$listOfValues)), 200)
modelWithTestDf <- MulEA::calculateHypergeometricTest(model = modelDfFromFile, sampleVector = sampleVector)
length(unique(unlist(modelWithTestDf$listOfValues)))

a2Z <- function() {
  allElements <- unique(unlist(modelWithTestDf$listOfValues))
  dataMatrix <- array(dim = c(length(modelWithTestDf$category), length(allElements)))
  for (i in 1:length(modelWithTestDf$category)) {
    concatenation <- c(modelWithTestDf[i,]$listOfValues[[1]], allElements)
    dataMatrix[i,] <- duplicated(concatenation)[(length(modelWithTestDf[i,]$listOfValues[[1]])+1):length(concatenation)]
  }
  simulationMatrix <- array(dim = c(length(modelWithTestDf$p.value), steps))
  for (j in 1:steps) {
    randomData <- logical(length(allElements))
    randomData[sample(length(allElements), size = length(sampleVector))] <- TRUE
    for (i in 1:length(modelWithTestDf$p.value)) {
      modelSampleIntersection <- dataMatrix[i,] & randomData
      q <- sum(modelSampleIntersection)
      m <- length(modelWithTestDf[i, 'listOfValues'][[1]])
      n <- length(allElements) - sum(dataMatrix[i,])
      k <- sum(randomData)
      # Why 1 - pvalue from test?
      simulationMatrix[i,j] <- 1 - phyper(q, m, n, k, lower.tail = FALSE, log.p = FALSE)
    }
  }
  simulationVector <- as.vector(simulationMatrix)
  simulationVector
}

modelSampleIntersection <- dataMatrix[1,] & randomData
q <- sum(modelSampleIntersection)
m <- length(modelWithTestDf[1, 'listOfValues'][[1]])
n <- length(allElements) - sum(dataMatrix[1,])
k <- sum(randomData)
# Why 1 - pvalue from test?
simulationMatrix[i,j] <- 1 - phyper(q, m, n, k, lower.tail = FALSE, log.p = FALSE)


aaa <- array(dim = c(10,3))
for (i in 1:10) {
  newOne <- array(dim = c(1,3))
  newOne[1, 1:3] <- FALSE
  newOne[1, sample(3,1)] <- TRUE
  aaa[i,] <- newOne
}

aaa[9,] & newOne

randomData[c(3,5,7)] <- TRUE
a2Xdata[1,3:10] <- TRUE
sum(randomData)
sum(a2Xdata[1,])
sum(a2Xdata[1,] & randomData)


dupa <- logical(length(allElements))
dupa[sample.int(length(allElements), size = length(sampleVector))] <- TRUE
sum(dupa)
dataMatrix[1:20, 1:10]


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

cat("\014")
for (i in c(100)) {
  for (j in c(200)) {
    steps <- i
    sampleVector <- sample(unique(unlist(modelDfFromFile$listOfValues)), j)
    print("**********************************")
    print(paste(i, j, sep = " - "))
    print(system.time(a2Xdata <- MulEA::gseaPermutationTest(modelWithTestDf, steps, sampleVector)))
    print(system.time(a2Ydata <- MulEA::gseaPermutationTestPlyr(modelWithTestDf, steps, sampleVector)))
    print(system.time(a2Zdata <- MulEA::gseaPermutationTestWithBinaryMatrix(modelWithTestDf, steps, sampleVector)))
    print(a2Xdata[1:10])
    print(a2Ydata[1:10])
    print(a2Zdata[1:10])
  }
}

steps <- 10

system.time(a2Zdata <- a2Z())

system.time(a2Ydata <- a2Y())


a2Xdata[1:20,1:10]


sum(a2Xdata == a2Ydata)


round(c(123.456, 123.4111156, 123.453336, 123.4563),digits=2)
