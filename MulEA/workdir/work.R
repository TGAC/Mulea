
muleaPkgDir <- find.package("MulEA")
modelDfFromFile <- MulEA::readGmtFileAsDF(gmtFilePath = paste(muleaPkgDir,"/example/model.gmt", sep = ""))
dataFromExperiment <- c("FBgn0004407", "FBgn0010438", "FBgn0003742", "FBgn0029709", "FBgn0030341", "FBgn0037044", "FBgn0002887", "FBgn0028434", "FBgn0030170", "FBgn0263831", "FBgn0261618", "FBgn0038704", "FBgn0000579")
dataFromExperimentScores <- c(0.09, 0.11, 0.15, 0.20, 0.21, 0.24, 0.28, 0.30, 0.45, 0.50, 0.53, 0.60, 0.61)

rankedBasedTestSubramanian <- RankedBasedTest(method = "Subramanian", gmt = modelDfFromFile, testData = dataFromExperiment, scores = dataFromExperimentScores)
rankedBasedTestSubramanianRes <- MulEA::runTest(rankedBasedTestSubramanian)


library(devtools)
install_github("TGAC/Mulea/tree/master/MulEA")




