
#' An S4 class to represent a ranked based tests in Mulea.
#'
#' @slot method A method from ranked methods to count results.
#' @slot gmt A data.frame representing GMT's reprezentation of model.
#' @slot testData A data from expeciment to analize accross model.
#' @slot scores A vectore of scores per testData.
#' @slot p A power of weight.
#' @slot numberOfPermutations A number of permutations used in renked test.
RankedBasedTest <- setClass("RankedBasedTest",
                            slots = list(
                              method = "character",
                              gmt = "data.frame",
                              testData = "character",
                              scores = "numeric",
                              p = "numeric",
                              numberOfPermutations = "numeric",
                              test = "function"
                            ))

setMethod("initialize", "RankedBasedTest",
          function(.Object,
                   method = character(),
                   gmt = data.frame(),
                   testData = character(),
                   scores = numeric(),
                   p = 1,
                   numberOfPermutations = 1000,
                   test = NULL,
                   ...) {

            .Object@method <- method
            .Object@gmt <- gmt
            .Object@testData <- testData
            .Object@scores <- scores
            .Object@p <- p
            .Object@numberOfPermutations <- numberOfPermutations

            .Object@test <- function(rankedBaseTestObject) {
              rankedTestRes <- NULL
              if (rankedBaseTestObject@method == "Subramanian") {
                subramanianTest <- SubramanianTest(gmt = rankedBaseTestObject@gmt,
                                                   testData = rankedBaseTestObject@testData,
                                                   scores = rankedBaseTestObject@scores,
                                                   p = rankedBaseTestObject@p,
                                                   numberOfPermutations = rankedBaseTestObject@numberOfPermutations)
                rankedTestRes <- runTest(subramanianTest)
              } else if (method == "KS") {
                ksTest <- KolmogorovSmirnovTest(gmt = rankedBaseTestObject@gmt,
                                                testData = rankedBaseTestObject@testData,
                                                numberOfPermutations = rankedBaseTestObject@numberOfPermutations)
                rankedTestRes <- runTest(ksTest)
              } else {
                warning("You have to choose method by typing method parameter to RankedBasedTest object. Choose from 'Subramanian' or 'KS'.")
              }
              rankedTestRes
            }

            .Object

          })

#' @describeIn RankedBasedTest Run test calculations.
#' @param testObject Object of s4 class represents Mulea Test.
setMethod("runTest",
          signature(testObject = "RankedBasedTest"),
          function(testObject) {
            testObject@test(testObject)
          })
