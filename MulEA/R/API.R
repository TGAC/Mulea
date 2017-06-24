
SetBasedTest <- setClass("SetBasedTest",
                                    slots = list(
                                      gmt = "data.frame",
                                      testData = "character",
                                      pool = "character",
                                      adjustMethod = "character",
                                      test = "function"
                                    ))

setMethod("initialize", "SetBasedTest",
          function(.Object,
                   gmt = data.frame(),
                   testData = character(),
                   pool = character(),
                   adjustMethod = character(),
                   test = NULL,
                   ...) {

            .Object@gmt <- gmt
            .Object@testData <- testData
            .Object@pool <- pool
            .Object@adjustMethod <- adjustMethod

            .Object@test <- function(setBaseTestObject) {
              print("INITIALIZE")
              muleaHypergeometricTest <- MuleaHypergeometricTest(gmt = setBaseTestObject@gmt, testData = setBaseTestObject@testData, pool = setBaseTestObject@pool)
              muleaHypergeometricTestRes <- runTest(muleaHypergeometricTest)
              if (length(setBaseTestObject@adjustMethod) != 0) {
                if (setBaseTestObject@adjustMethod == "EszterMethod") {
                  # adjustResult <- data.frame(modelWithTestsResults, gseaPermutationTest(modelWithTestDf = modelWithTestsResults, steps = steps, sampleVector = sampleVector, poolVector = poolVector))
                } else {
                  muleaHypergeometricTestRes <- data.frame(muleaHypergeometricTestRes, "q.value" = p.adjust(muleaHypergeometricTestRes$p.value, method = adjustMethod))
                }
              }
              muleaHypergeometricTestRes
            }

            .Object

          })

setMethod("runTest",
          signature(testObject = "SetBasedTest"),
          function(testObject) {
            print("RUN TEST on SetBasedTest")
            testObject@test(testObject)
          })
