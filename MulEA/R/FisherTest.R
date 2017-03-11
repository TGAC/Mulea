setClass("muleaFisherTest",
         slots = list(
           testData = "character",
           test = "function"
         ))

setMethod("initialize", "muleaFisherTest",
          function(.Object,
                   testData = character(),
                   test = NULL,
                   ...) {

            .Object@testData <- testData

            .Object@test <- function(dataObject, testObject) {
              MulEA::calculateFisherTest(model = dataObject@gmt, sampleVector = testObject@testData)
            }

            .Object

          })
