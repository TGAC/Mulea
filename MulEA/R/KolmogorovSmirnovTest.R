
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
                                  N <- length(testObject@testData)
                                  na <- length(categoryValues)
                                  if(na == 0 || na == N)
                                    return(1)
                                  a <- match(categoryValues, testObject@testData)
                                  # a <- x.a[!is.na(x.a)]
                                  a <- a[!is.na(a)]
                                  if ( length(a) == 0 ) {
                                    return(1)
                                  }
                                  # return(ks.test(a, seq_len(N)[-a], alternative = "greater")$p.value)
                                  ks.test(a, seq_len(N)[-a])$p.value

                                })

              resultDf <- data.frame(dataObject@gmt, "p.value" = pvalues)
              resultDf

            }

            .Object

          })
