
################################ Final 1.0.0 work with MulEA DB ################################


# Create SQLite DB on local machine as file or in memory.
creationOfLocalDB <- MulEA::startLocalDatabase("/home/koralgooll/doktorat/Rpackages/muleaDb/")
creationOfLocalDB <- MulEA::startLocalDatabase(":memory:")


# In case of corrupted data - DB is broken. (You can not reset R session, lock on DB file)
stopDbResults <- MulEA::stopLocalDatabase()


# Read model from file.
muleaPkgDir <- find.package("MulEA")
modelDfFromFile <- MulEA::readGmtFileAsDF(gmtFilePath = paste(muleaPkgDir,"/example/model.gmt", sep = ""))


# Add model to database.
MulEA::addModelToLocalDatabase(model = modelDfFromFile, taxonomy_id = 9001, model_source = "GO", version = 0)


# Get model from local (future: global) database. It is possible in two forms DF and list.
modelDfFromLocalDB <- modelDfFromFile
modelDfFromLocalDB <- MulEA::getModelFromLocalDatabaseAsDf(taxonomy_id = 9001, model_source = "GO", version = 0)
modelListFromLocalDB <- MulEA::getModelFromLocalDatabaseAsList(taxonomy_id = 9001, model_source = "GO", version = 0)


# Remove unused model from local database.
MulEA::removeModelFromLocalDatabase(taxonomy_id = 9001, model_source = "GO", version = 0)


# Add model again from DF which is from local DB.
MulEA::addModelToLocalDatabase(model = modelDfFromLocalDB, taxonomy_id = 9001, model_source = "GO", version = 0)


# Save model to file. You can do it from level of DF and local DB.
MulEA::saveModelFromDataFrameToGmtFile(modelDF = modelDfFromLocalDB, gmtFilePath = "/home/koralgooll/doktorat/Rpackages/mulea/Mulea/example/fromDf.gmt")
MulEA::saveModelFromLocalDatabaseToFile(taxonomy_id = 9001, model_source = "GO", version = 0, gmtFilePath = "/home/koralgooll/doktorat/Rpackages/mulea/Mulea/example/fromDb.gmt")



################################ Final 1.0.0 object approach in tests ################################


# Clear screen
cat("\014")


# Read example model and create experiment data
muleaPkgDir <- find.package("MulEA")
modelDfFromFile <- MulEA::readGmtFileAsDF(gmtFilePath = paste(muleaPkgDir,"/example/model.gmt", sep = ""))
dataFromExperiment <- c("FBgn0004407", "FBgn0010438", "FBgn0003742", "FBgn0029709", "FBgn0030341", "FBgn0037044", "FBgn0002887", "FBgn0028434", "FBgn0030170", "FBgn0263831")
dataFromExperimentScores <- c(0.09, 0.11, 0.15, 0.20, 0.21, 0.24, 0.28, 0.30, 0.45, 0.50)
dataFromExperimentPool <- unique(c(c("FBgn0033690", "FBgn0261618", "FBgn0004407", "FBgn0010438", "FBgn0032154", "FBgn0039930", "FBgn0040268", "FBgn0013674",
                                   "FBgn0037008", "FBgn0003116", "FBgn0037743", "FBgn0035401", "FBgn0037044", "FBgn0051005", "FBgn0026737", "FBgn0026751",
                                   "FBgn0038704", "FBgn0002887", "FBgn0028434", "FBgn0030170", "FBgn0263831", "FBgn0000579"),
                                 c("FBgn0066666", "FBgn0000000", "FBgn0099999", "FBgn0011111", "FBgn0022222", "FBgn0777777", "FBgn0333333", "FBgn0003742",
                                   "FBgn0029709", "FBgn0030341")))


# SetBaseTest approach
setBasedTest <- SetBasedTest(gmt = modelDfFromFile, testData = dataFromExperiment)
setBasedTestWithPool <- SetBasedTest(gmt = modelDfFromFile, testData = dataFromExperiment, pool = dataFromExperimentPool)
setBasedTestWithPoolAndAdjust <- SetBasedTest(gmt = modelDfFromFile, testData = dataFromExperiment, pool = dataFromExperimentPool, adjustMethod = "BH")

setBasedTestRes <- MulEA::runTest(setBaseTest)
setBasedTestWithPoolRes <- MulEA::runTest(setBaseTestWithPool)
setBasedTestWithPoolAndAdjustRes <- MulEA::runTest(setBaseTestWithPoolAndAdjust)

setBasedTestRes
setBasedTestWithPoolRes
setBasedTestWithPoolAndAdjustRes


# RankedBasedTest approach
rankedBasedTestKs <- RankedBasedTest(method = "KS", gmt = modelDfFromFile, testData = dataFromExperiment)
rankedBasedTestSubramanian <- RankedBasedTest(method = "Subramanian", gmt = modelDfFromFile, testData = dataFromExperiment, scores = dataFromExperimentScores)

rankedBasedTestKsRes <- MulEA::runTest(rankedBasedTestKs)
rankedBasedTestSubramanianRes <- MulEA::runTest(rankedBasedTestSubramanian)

rankedBasedTestKsRes
rankedBasedTestSubramanianRes


