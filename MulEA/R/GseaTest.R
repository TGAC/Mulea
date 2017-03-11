
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
