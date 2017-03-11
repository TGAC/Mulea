
setClass("muleaHypergeometricTest",
         slots = list(
           testData = "character",
           pool = "character",
           test = "function"
         ))

setMethod("initialize", "muleaHypergeometricTest",
          function(.Object,
                   testData = character(),
                   pool = character(),
                   test = NULL,
                   ...) {

            .Object@testData <- testData
            .Object@pool <- pool

            .Object@test <- function(dataObject, testObject) {
              MulEA::calculateHypergeometricTest(model = dataObject@gmt, sampleVector = testObject@testData, poolVector = testObject@pool)
            }

            .Object

          })
