################################ Classes with initialization methods. ###################################

setClass("muleaData",
         slots = list(
           model = "data.frame",
           taxonomyId = "character",
           modelSource = "character",
           version = "character",
           scientificName = "character",
           commonEnglishName = "character",
           description = "character",
           modelBinaryMatrix = "data.frame"
         ))

setMethod("initialize", "muleaData",
          function(.Object,
                   model = data.frame(),
                   taxonomyId = character(),
                   modelSource = character(),
                   version = character(),
                   scientificName = character(),
                   commonEnglishName = character(),
                   description = character(),
                   # TODO : Implement.
                   modelBinaryMatrix = data.frame(),
                   ...) {

            .Object@model <- model
            .Object@taxonomyId <- taxonomyId
            .Object@modelSource <- modelSource
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
           samples = "character",
           scores = "numeric",
           test = "function"
         ))

setMethod("initialize", "muleaKolmogorovSmirnovTest",
          function(.Object,
                   samples = character(),
                   scores = numeric(),
                   test = NULL,
                   ...) {

            .Object@samples <- samples
            .Object@scores <- scores

            .Object@test <- function(dataObject, testObject) {

              pvalues <- sapply(dataObject@model$listOfValues,
                                function(categoryValues) {
                                  N <- length(testObject@samples)
                                  na <- length(categoryValues)
                                  if(na == 0 || na == N)
                                    return(1)
                                  a <- match(categoryValues, testObject@samples)
                                  # a <- x.a[!is.na(x.a)]
                                  a <- a[!is.na(a)]
                                  if(length(a) == 0)
                                    return(1)

                                  # return(ks.test(a, seq_len(N)[-a], alternative = "greater")$p.value)
                                  ks.test(a, seq_len(N)[-a])$p.value

                                })

              resultDf <- data.frame(dataObject@model, pvalues)
              resultDf

            }

            .Object

          })


setClass("muleaHypergeometricTest",
         slots = list(
           samples = "character",
           test = "function"
         ))

setMethod("initialize", "muleaHypergeometricTest",
          function(.Object,
                   samples = character(),
                   test = NULL,
                   ...) {

            .Object@samples <- samples

            .Object@test <- function(dataObject, testObject) {
              MulEA::calculateHypergeometricTest(model = dataObject@model, sampleVector = testObject@samples)
            }

            .Object

          })

setClass("muleaFisherTest",
         slots = list(
           samples = "character",
           test = "function"
         ))

setMethod("initialize", "muleaFisherTest",
          function(.Object,
                   samples = character(),
                   test = NULL,
                   ...) {

            .Object@samples <- samples

            .Object@test <- function(dataObject, testObject) {
              MulEA::calculateFisherTest(model = dataObject@model, sampleVector = testObject@samples)
            }

            .Object

          })


setClass("muleaChiSquaredTest",
         slots = list(
           samples = "character",
           test = "function"
         ))

setMethod("initialize", "muleaChiSquaredTest",
          function(.Object,
                   samples = character(),
                   test = NULL,
                   ...) {

            .Object@samples <- samples

            .Object@test <- function(dataObject, testObject) {
              MulEA::calculateChiSquaredTest(model = dataObject@model, sampleVector = testObject@samples)
            }

            .Object

          })

