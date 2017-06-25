
SubramanianTest <- setClass("SubramanianTest",
                            slots = list(
                              gmt = "data.frame",
                              testData = "character",
                              scores = "numeric",
                              p = "numeric",
                              numberOfPermutations = "numeric",
                              test = "function"
                            ))

setMethod("initialize", "SubramanianTest",
          function(.Object,
                   gmt = data.frame(),
                   testData = character(),
                   scores = numeric(),
                   p = 1,
                   numberOfPermutations = 1000,
                   test = NULL,
                   ...) {

            .Object@gmt <- gmt
            .Object@testData <- testData
            .Object@scores <- scores
            .Object@p <- p
            .Object@numberOfPermutations <- numberOfPermutations

            .Object@test <- function(testObject) {

              listmodelDfFromFile <- testObject@gmt$listOfValues
              names(listmodelDfFromFile) <- testObject@gmt$ontologyId

              samplesToAnalisys <- testObject@scores
              names(samplesToAnalisys) <- testObject@testData

              fgseaRes <- fgsea(pathways = listmodelDfFromFile,
                                stats = samplesToAnalisys,
                                gseaParam = testObject@p, nperm = testObject@numberOfPermutations)

              resultDf <- merge(testObject@gmt, fgseaRes, by.x = "ontologyId", by.y = "pathway", all = TRUE)[c("ontologyId", "ontologyName", "listOfValues", "pval")]
              colnames(resultDf) <- c("ontologyId", "ontologyName", "listOfValues", "p.value")
              resultDf
            }

            .Object

          })

setMethod("runTest",
          signature(testObject = "SubramanianTest"),
          function(testObject) {
            testObject@test(testObject)
          })
