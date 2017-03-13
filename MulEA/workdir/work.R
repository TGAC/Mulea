cat("\014")

muleaPkgDir <- find.package("MulEA")
modelDfFromFile <- MulEA::readGmtFileAsDF(gmtFilePath = paste(muleaPkgDir,"/example/model.gmt", sep = ""))
muleaDataObject <- new(Class = "muleaData", gmt = modelDfFromFile)

dataFromExperimentPool <- unique(c(unlist(muleaDataObject@gmt$listOfValues), c("FBgn0066666", "FBgn0000000", "FBgn0099999", "FBgn0011111", "FBgn0022222", "FBgn0777777", "FBgn0333333", "FBgn0003742", "FBgn0029709", "FBgn0030341")))
dataFromExperimentT <- c("FBgn0004407", "FBgn0010438", "FBgn0037044", "FBgn0002887", "FBgn0028434", "FBgn0030170", "FBgn0263831")

muleaHypergeometricTestObject <- new("muleaHypergeometricTest", testData = dataFromExperimentT)
muleaHypergeometricTestObjectWithPool <- new("muleaHypergeometricTest", testData = dataFromExperimentT, pool = dataFromExperimentPool)

hTestRes <- MulEA::runTest(muleaDataObject, muleaHypergeometricTestObject)
hTestResWithPool <- MulEA::runTest(muleaDataObject, muleaHypergeometricTestObjectWithPool)

gseaAdjustment <- MulEA::adjustPvaluesForMultipleComparisons(modelWithTestsResults = hTestRes, sampleVector = dataFromExperimentT,
                                                             steps = 10, adjustMethod = "GSEA")

gseaAdjustmentWithPool <- MulEA::adjustPvaluesForMultipleComparisons(modelWithTestsResults = hTestResWithPool, sampleVector = dataFromExperimentT,
                                                                     steps = 10, poolVector = dataFromExperimentPool, adjustMethod = "GSEA")








