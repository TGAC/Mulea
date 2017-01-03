#Flow for Vignettes:

#Create SQLite DB on local machine as file or in memory.
creationOfLocalDB <- MulEA::startLocalDatabase("/home/koralgooll/doktorat/Rpackages/mulea/")
creationOfLocalDB <- MulEA::startLocalDatabase(":memory:")
#I case of corrupted data - DB is broken. (You can not reset R session, lock on DB file)
stopDbResults <- MulEA::stopLocalDatabase()

#Read model from file.
modelDfFromFile <- MulEA::readGmtFileAsDF(gmtFilePath = "/home/koralgooll/doktorat/Rpackages/mulea/Mulea/example/GO2allGenes_FBgnIDs_v2.gmt")

#Add model to database.
MulEA::addModelToLocalDatabase(model = modelDfFromFile, taxonomy_id = 9001, model_source = "GO", version = 0)

#Get model from local (future: global) database. It is possible in two forms DF and list.
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
fTestResults <- MulEA::calculateFisherTest(model = modelDfFromFile, sampleVector = dataFromExperiment)
chTestResults <- MulEA::calculateChiSquaredTest(model = modelDfFromLocalDB, sampleVector = dataFromExperiment)

#Adjust P-values for Multiple Comparisons.
hTestResultsWithAdjustment <- MulEA::calculateHypergeometricTest(model = modelDfFromLocalDB, sampleVector = dataFromExperiment, adjustMethod = "BH")
fTestResultsWithAdjustment <- MulEA::calculateFisherTest(model = modelDfFromFile, sampleVector = dataFromExperiment, adjustMethod = "bonferroni")
chTestResultsWithAdjustment <- MulEA::calculateChiSquaredTest(model = modelDfFromLocalDB, sampleVector = dataFromExperiment, adjustMethod = "BY")
