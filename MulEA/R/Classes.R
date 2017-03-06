################################ Classes with initialization methods. ###################################

setClass("muleaData",
         slots = list(
           gmt = "data.frame",
           taxID = "character",
           ontologyName = "character",
           version = "character",
           scientificName = "character",
           commonEnglishName = "character",
           description = "character",
           modelBinaryMatrix = "data.frame" #TODO : this will be removed, privat
         ))

setMethod("initialize", "muleaData",
          function(.Object,
                   gmt = data.frame(),
                   taxID = character(),
                   ontologyName = character(),
                   version = character(),
                   scientificName = character(),
                   commonEnglishName = character(),
                   description = character(),
                   # TODO : Implement.
                   modelBinaryMatrix = data.frame(),
                   ...) {

            .Object@gmt <- gmt
            .Object@taxID <- taxID
            .Object@ontologyName <- ontologyName
            .Object@version <- version
            .Object@scientificName <- scientificName
            .Object@commonEnglishName <- commonEnglishName
            .Object@description <- description

            # TODO : An algorithm to count it from clean model.
            .Object@modelBinaryMatrix <- data.frame(a = c(1,2), b = c("asa", "bsb"))

            .Object

          })


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

              resultDf <- data.frame(dataObject@gmt, pvalues)
              resultDf

            }

            .Object

          })


setClass("rankedGseaTest",
         slots = list(
           testData = "character",
           scores = "numeric",
           test = "function",
           p = "numeric",
           numberOfPermutations = "numeric"
         ))

setMethod("initialize", "rankedGseaTest",
          function(.Object,
                   testData = character(),
                   scores = numeric(),
                   p = 1,
                   test = NULL,
                   numberOfPermutations = 1000,
                   ...) {

            .Object@testData <- testData
            .Object@scores <- scores
            .Object@p <- p
            .Object@numberOfPermutations <- numberOfPermutations

            .Object@test <- function(dataObject, testObject) {

              listmodelDfFromFile <- dataObject@gmt$listOfValues
              names(listmodelDfFromFile) <- dataObject@gmt$category

              samplesToAnalisys <- testObject@scores
              names(samplesToAnalisys) <- testObject@testData

              fgseaRes <- fgsea(pathways = listmodelDfFromFile,
                                              stats = samplesToAnalisys,
                                              gseaParam = testObject@p, nperm = testObject@numberOfPermutations)

              resultDf <- merge(dataObject@gmt, fgseaRes, by.x = "category", by.y = "pathway", all = TRUE)[c("category", "description", "listOfValues", "pval")]
              resultDf

            }

            .Object

          })


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


setClass("muleaChiSquaredTest",
         slots = list(
           testData = "character",
           test = "function"
         ))

setMethod("initialize", "muleaChiSquaredTest",
          function(.Object,
                   testData = character(),
                   test = NULL,
                   ...) {

            .Object@testData <- testData

            .Object@test <- function(dataObject, testObject) {
              MulEA::calculateChiSquaredTest(model = dataObject@gmt, sampleVector = testObject@testData)
            }

            .Object

          })

