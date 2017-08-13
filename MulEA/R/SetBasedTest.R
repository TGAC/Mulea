
#' An S4 class to represent a set based tests in Mulea.
#'
#' @slot gmt A data.frame representing GMT's reprezentation of model.
#' @slot testData A data from expeciment to analize accross model.
#' @slot pool A background data to count test.
#' @slot adjustMethod A type of algorithm used to adjust values.
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

#' @describeIn SetBasedTest runs test calculations.
#' @param testObject Object of s4 class represents Mulea Test.
setMethod("runTest",
          signature(testObject = "SetBasedTest"),
          function(testObject) {
            testObject@test(testObject)
          })
