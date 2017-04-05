setClass("muleaFisherTest",
         slots = list(
           testData = "character",
           pool = "character",
           test = "function"
         ))

setMethod("initialize", "muleaFisherTest",
          function(.Object,
                   testData = character(),
                   pool = character(),
                   test = NULL,
                   ...) {

            .Object@testData <- testData
            .Object@pool <- pool

            .Object@test <- function(dataObject, testObject) {
              MulEA::calculateFisherTest(model = dataObject@gmt, sampleVector = testObject@testData, poolVector = testObject@pool)
            }

            .Object

          })
