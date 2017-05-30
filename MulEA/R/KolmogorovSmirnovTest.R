
setClass("muleaKolmogorovSmirnovTest",
         slots = list(
           testData = "character",
           test = "function"
         ))

setMethod("initialize", "muleaKolmogorovSmirnovTest",
          function(.Object,
                   testData = character(),
                   test = NULL,
                   ...) {

            .Object@testData <- testData

            .Object@test <- function(dataObject, testObject) {
              pvalues <- sapply(dataObject@gmt$listOfValues,
                                function(categoryValues) {
                                  a <- match(categoryValues, testObject@testData)
                                  if (length(a[!is.na(a)]) == 0 ) {
                                    return(1.0)
                                  }
                                  ks.test(a, seq_len(length(testObject@testData)))$p.value
                                })
              resultDf <- data.frame(dataObject@gmt, "p.value" = pvalues)
              resultDf
            }

            .Object

          })
