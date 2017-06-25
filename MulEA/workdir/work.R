
muleaPkgDir <- find.package("MulEA")
modelDfFromFile <- MulEA::readGmtFileAsDF(gmtFilePath = paste(muleaPkgDir,"/example/model.gmt", sep = ""))
dataFromExperiment <- c("FBgn0004407", "FBgn0010438", "FBgn0003742", "FBgn0029709", "FBgn0030341", "FBgn0037044", "FBgn0002887", "FBgn0028434", "FBgn0030170", "FBgn0263831")


matchedFromModel <- match(modelDfFromFile[2,]$listOfValues[[1]], dataFromExperiment)
matchedFromModelDist <- matchedFromModel[!is.na(matchedFromModel)]
pvalues <- aaply(.data = 1:2000, .margins = 1, .fun = function(element) {
  randomFromExperimentDist <- sort(sample(seq_len(length(dataFromExperiment)), length(matchedFromModelDist)))
  ks.test(matchedFromModelDist, randomFromExperimentDist)$p.value
})
mean(pvalues)



length(matchedFromModelDist)

a <- match(categoryValues, testObject@testData)


library(plyr)
pvalues <- aaply(.data = 1:2000, .margins = 1, .fun = function(element) {

})


sample(c(1,2), 3)





