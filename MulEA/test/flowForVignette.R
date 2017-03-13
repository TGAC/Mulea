#Flow for Vignettes:



##############################Functions################################

#Create SQLite DB on local machine as file or in memory.
creationOfLocalDB <- MulEA::startLocalDatabase("/home/koralgooll/doktorat/Rpackages/muleaDb/")
creationOfLocalDB <- MulEA::startLocalDatabase(":memory:")
#I case of corrupted data - DB is broken. (You can not reset R session, lock on DB file)
stopDbResults <- MulEA::stopLocalDatabase()

#Read model from file.
muleaPkgDir <- find.package("MulEA")
modelDfFromFile <- MulEA::readGmtFileAsDF(gmtFilePath = paste(muleaPkgDir,"/example/model.gmt", sep = ""))

#Add model to database.
MulEA::addModelToLocalDatabase(model = modelDfFromFile, taxonomy_id = 9001, model_source = "GO", version = 0)

#Get model from local (future: global) database. It is possible in two forms DF and list.
modelDfFromLocalDB <- modelDfFromFile
modelDfFromLocalDB <- MulEA::getModelFromLocalDatabaseAsDf(taxonomy_id = 9001, model_source = "GO", version = 0)
modelListFromLocalDB <- MulEA::getModelFromLocalDatabaseAsList(taxonomy_id = 9001, model_source = "GO", version = 0)

#Remove unused model from local database.
MulEA::removeModelFromLocalDatabase(taxonomy_id = 9001, model_source = "GO", version = 0)

#Add model again from DF which is from local DB.
MulEA::addModelToLocalDatabase(model = modelDfFromLocalDB, taxonomy_id = 9001, model_source = "GO", version = 0)

#Save model to file. You can do it from level of DF and local DB.
MulEA::saveModelFromDataFrameToGmtFile(modelDF = modelDfFromLocalDB, gmtFilePath = "/home/koralgooll/doktorat/Rpackages/mulea/Mulea/example/fromDf.gmt")
MulEA::saveModelFromLocalDatabaseToFile(taxonomy_id = 9001, model_source = "GO", version = 0, gmtFilePath = "/home/koralgooll/doktorat/Rpackages/mulea/Mulea/example/fromDb.gmt")

#You data from experiment.
dataFromExperiment <- c("FBgn0004407", "FBgn0010438", "FBgn0003742", "FBgn0029709", "FBgn0030341", "FBgn0037044", "FBgn0002887")

#Statistical tests.
gTestResults <- MulEA::calculateGSEATest(model = modelDfFromFile, sampleVector = dataFromExperiment, modelBaseVector = character(0))
hTestResults <- MulEA::calculateHypergeometricTest(model = modelDfFromLocalDB, sampleVector = dataFromExperiment)
hTestResultsWithPool <- MulEA::calculateHypergeometricTest(model = modelDfFromLocalDB, sampleVector = dataFromExperiment, poolVector = dataFromExperimentPool)
fTestResults <- MulEA::calculateFisherTest(model = modelDfFromFile, sampleVector = dataFromExperiment)
chTestResults <- MulEA::calculateChiSquaredTest(model = modelDfFromLocalDB, sampleVector = dataFromExperiment)

#Adjust P-values for Multiple Comparisons.
defaultAdjustment <- MulEA::adjustPvaluesForMultipleComparisons(modelWithTestsResults = hTestResults, sampleVector = dataFromExp)
gseaAdjustment <- MulEA::adjustPvaluesForMultipleComparisons(modelWithTestsResults = hTestResults, sampleVector = dataFromExperiment, steps = 2, adjustMethod = "GSEA")
gseaAdjustmentWithPool <- MulEA::adjustPvaluesForMultipleComparisons(modelWithTestsResults = hTestResultsWithPool, sampleVector = dataFromExperiment, poolVector = dataFromExperimentPool, steps = 2, adjustMethod = "GSEA")
bhAdjustment <- MulEA::adjustPvaluesForMultipleComparisons(modelWithTestsResults = hTestResults, sampleVector = dataFromExp, adjustMethod = "BH")



##############################Objects################################
cat("\014")
#Object approach. :)
muleaPkgDir <- find.package("MulEA")
modelDfFromFile <- MulEA::readGmtFileAsDF(gmtFilePath = paste(muleaPkgDir,"/example/model.gmt", sep = ""))
str(modelDfFromFile$listOfValues)

dataFromExperimentPool <- unique(c(unlist(muleaDataObject@gmt$listOfValues), c("FBgn0066666", "FBgn0000000", "FBgn0099999", "FBgn0011111", "FBgn0022222", "FBgn0777777", "FBgn0333333", "FBgn0003742", "FBgn0029709", "FBgn0030341")))
dataFromExperimentF <- c("FBgn0004407", "FBgn0010438", "FBgn0003742", "FBgn0029709", "FBgn0030341", "FBgn0037044", "FBgn0002887", "FBgn0028434", "FBgn0030170", "FBgn0263831")
dataFromExperimentT <- c("FBgn0004407", "FBgn0010438", "FBgn0037044", "FBgn0002887", "FBgn0028434", "FBgn0030170", "FBgn0263831")
dataFromExperimentScores <- c(0.09, 0.11, 0.15, 0.20, 0.21, 0.24, 0.28, 0.30, 0.45, 0.50)

muleaDataObject <- new(Class = "muleaData", gmt = modelDfFromFile)

muleaKolmogorovSmirnovTestObject <- new("muleaKolmogorovSmirnovTest", testData = dataFromExperimentF)
rankedGseaTestObject <- new("rankedGseaTest", testData = dataFromExperimentF, scores = dataFromExperimentScores, p = 3, numberOfPermutations = 10000)

muleaHypergeometricTestObject <- new("muleaHypergeometricTest", testData = dataFromExperimentT)
muleaHypergeometricTestObjectWithPool <- new("muleaHypergeometricTest", testData = dataFromExperimentT, pool = dataFromExperimentPool)
muleaFisherTestObject <- new("muleaFisherTest", testData = dataFromExperimentT)
muleaChiSquaredTestObject <- new("muleaChiSquaredTest", testData = dataFromExperimentT)

ksTestRes <- MulEA::runTest(muleaDataObject, muleaKolmogorovSmirnovTestObject)
ksWithRankTestRes <- MulEA::runTest(muleaDataObject, rankedGseaTestObject)
hTestRes <- MulEA::runTest(muleaDataObject, muleaHypergeometricTestObject)
hTestResWithPool <- MulEA::runTest(muleaDataObject, muleaHypergeometricTestObjectWithPool)
fTestRes <- MulEA::runTest(muleaDataObject, muleaFisherTestObject)
chTestRes <- MulEA::runTest(muleaDataObject, muleaChiSquaredTestObject)


gseaAdjustment <- MulEA::adjustPvaluesForMultipleComparisons(modelWithTestsResults = hTestRes, sampleVector = dataFromExperimentT,
                                                             steps = 100, adjustMethod = "GSEA")
gseaAdjustmentWithPool <- MulEA::adjustPvaluesForMultipleComparisons(modelWithTestsResults = hTestResWithPool, sampleVector = dataFromExperimentT,
                                                                     steps = 100, poolVector = dataFromExperimentPool, adjustMethod = "GSEA")


decoratedTestRes <- MulEA::runTest(new(Class = "muleaData", model = hTestRes), muleaKolmogorovSmirnovTestObject)


duplicated(c(unlist(hTestRes$listOfValues), dataFromExperiment))
dataFromExperiment <- c(dataFromExperiment[1:2], dataFromExperiment[6:10])

