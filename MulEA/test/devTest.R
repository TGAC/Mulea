smallResults <- MulEA::runGSEA(
    databaseFilePath = "/home/koralgooll/experiments/GSEA/C/GSEA/Database/small_db.txt"
    , populationFilePath = "/home/koralgooll/experiments/GSEA/C/GSEA/Database/small_pool.txt"
    , sampleFilePath = "/home/koralgooll/experiments/GSEA/C/GSEA/Database/small_sample.txt")

smallResultsTabSep <- MulEA::runGSEA(
  databaseFilePath = "/home/koralgooll/experiments/GSEA/C/GSEA/Database/small_db_tab_sep.txt"
  , populationFilePath = "/home/koralgooll/experiments/GSEA/C/GSEA/Database/small_pool.txt"
  , sampleFilePath = "/home/koralgooll/experiments/GSEA/C/GSEA/Database/small_sample.txt")

all.equal(smallResults, smallResultsTabSep)

mediumResults <- MulEA::runGSEA(
    databaseFilePath = "/home/koralgooll/experiments/GSEA/C/GSEA/Database/medium_db.txt"
    , populationFilePath = "/home/koralgooll/experiments/GSEA/C/GSEA/Database/medium_pool.txt"
    , sampleFilePath = "/home/koralgooll/experiments/GSEA/C/GSEA/Database/medium_sample.txt")

bigResults <- MulEA::runGSEA(
    databaseFilePath = "/home/koralgooll/experiments/GSEA/C/GSEA/Database/big_db.txt"
    , populationFilePath = "/home/koralgooll/experiments/GSEA/C/GSEA/Database/big_pool.txt"
    , sampleFilePath = "/home/koralgooll/experiments/GSEA/C/GSEA/Database/big_sample.txt")

bigResultsTabSep <- MulEA::runGSEA(
  databaseFilePath = "/home/koralgooll/experiments/GSEA/C/GSEA/Database/big_db_tab_sep.txt"
  , populationFilePath = "/home/koralgooll/experiments/GSEA/C/GSEA/Database/big_pool.txt"
  , sampleFilePath = "/home/koralgooll/experiments/GSEA/C/GSEA/Database/big_sample.txt")


creationOfLocalDB <- MulEA::startLocalDatabase("/home/koralgooll/doktorat/Rpackages/mulea/")
creationOfLocalDB <- MulEA::startLocalDatabase(":memory:")
#I case of corrupted data - DB is broken.
stopDbResults <- MulEA::stopLocalDatabase()

get("databaseLocalization", envir = .GlobalEnv)
get("databaseConnection", envir = .GlobalEnv)

model <- MulEA::readGmtFileAsDF(gmtFilePath = "/home/koralgooll/doktorat/Rpackages/mulea/Mulea/example/REACTOME_database_dmel_uniq_PROPER.txt")
modelFromEszter <- MulEA::readGmtFileAsDF(gmtFilePath = "/home/koralgooll/doktorat/Rpackages/mulea/Mulea/example/GO2allGenes_FBgnIDs_v2.gmt")
model2FromEszter <- MulEA::readGmtFileAsDF(gmtFilePath = "/home/koralgooll/doktorat/Rpackages/mulea/Mulea/example/GO2allGenes_FBgnIDs_v3.gmt")
model3FromEszter <- MulEA::readGmtFileAsDF(gmtFilePath = "/home/koralgooll/doktorat/Rpackages/mulea/Mulea/example/c2.cgp.v5.1.symbols.gmt")
cat("\014")
addToLocalDB20 <- MulEA::addModelToLocalDatabase(model = model,
                                                taxonomy_id = 9006, model_source = 'File',
                                                version = 0, scientific_name = "real animal",
                                                description = 'Test on real data')

addToLocalDB20x <- MulEA::addModelToLocalDatabase(model = modelFromEszter,
                                                 taxonomy_id = 9006, model_source = 'Reactome',
                                                 version = 0, scientific_name = "some real animal",
                                                 description = 'Just test case')

addToLocalDB20 <- MulEA::addModelToLocalDatabase(model = modelFromEszter,
                                                 taxonomy_id = 9001, model_source = 'Reactome',
                                                 version = 0, scientific_name = "some real animal",
                                                 description = 'Just test case')

addToLocalDB20 <- MulEA::addModelToLocalDatabase(model = model2FromEszter,
                                                 taxonomy_id = 9016, model_source = 'File',
                                                 version = 0, scientific_name = "R animal",
                                                 description = 'To REMOVE!')

addToLocalDB20 <- MulEA::addModelToLocalDatabase(model = model3FromEszter,
                                                 taxonomy_id = 9006, model_source = 'GO',
                                                 version = 0, scientific_name = "some real animal",
                                                 description = 'Just test case')


removeFromLocalDB1 <- MulEA::removeModelFromLocalDatabase(taxonomy_id = 9006, model_source = 'Reactome', version = 0)
removeFromLocalDB1 <- MulEA::removeModelFromLocalDatabase(taxonomy_id = 9016, model_source = 'File', version = 0)

getData1 <- MulEA::saveModelFromLocalDatabaseToFile(gmtFilePath = "testSave1.gmt",
                                                   taxonomy_id = 9006, model_source = 'Reactome', version = 0)
dfFromDB1 <- MulEA::getModelFromLocalDatabaseAsDf(taxonomy_id = 9006, model_source = 'Reactome', version = 0)
cosFromDB1 <- MulEA::getModelFromLocalDatabase(taxonomy_id = 9006, model_source = 'Reactome', version = 0)


getDataX <- MulEA::saveModelFromLocalDatabaseToFile(gmtFilePath = "smallTest.gmt",
                                                    taxonomy_id = 1000, model_source = 'GSEA', version = 0)
getData2 <- MulEA::saveModelFromLocalDatabaseToFile(gmtFilePath = "testSave2.gmt",
                                                   taxonomy_id = 9016, model_source = 'File', version = 0)
getData3 <- MulEA::saveModelFromLocalDatabaseToFile(gmtFilePath = "testSave3.gmt",
                                                   taxonomy_id = 9006, model_source = 'GO', version = 0)


modelFromSave1 <- MulEA::readGmtFileAsDF(gmtFilePath = "testSave1.gmt")
modelFromSave2 <- MulEA::readGmtFileAsDF(gmtFilePath = "testSave2.gmt")
modelFromSave3 <- MulEA::readGmtFileAsDF(gmtFilePath = "testSave3.gmt")


modeflFromEszterDf <- MulEA::getModelFromLocalDatabaseAsDf(taxonomy_id = 9001,
                                                           model_source = 'Reactome',
                                                           version = 0)
myDataFromExperiment <- c("FBgn0004407", "FBgn0010438", "FBgn0003742", "FBgn0029709", "FBgn0030341", "FBgn0037044", "FBgn0002887")

smallResultsTabSep <- MulEA::runGSEA(
  databaseFilePath = ""
  , populationFilePath = "/home/koralgooll/experiments/GSEA/C/GSEA/Database/small_pool.txt"
  , sampleFilePath = "/home/koralgooll/experiments/GSEA/C/GSEA/Database/small_sample.txt")

smallGseaModelDF <- MulEA::readGmtFileAsDF(gmtFilePath = "/home/koralgooll/experiments/GSEA/C/GSEA/Database/small_db_tab_sep.txt")

MulEA::addModelToLocalDatabase(model = smallGseaModelDF, taxonomy_id = 1000, model_source = 'GSEA', version = 0)
MulEA::removeModelFromLocalDatabase(taxonomy_id = 1000, model_source = 'GSEA', version = 0)

gseaModelFromDBasDF <- MulEA::getModelFromLocalDatabaseAsDf(taxonomy_id = 1000, model_source = 'GSEA', version = 0)

gTestResults <- MulEA::calculateGSEATest(model = gseaModelFromDBasDF, sampleVector = c("1","2","3"), modelBaseVector = character(0))

hTestResults <- MulEA::calculateHypergeometricTest(model = modeflFromEszterDf,
                                                   sampleVector = myDataFromExperiment)
fTestResults <- MulEA::calculateFisherTest(model = modeflFromEszterDf,
                                           sampleVector = myDataFromExperiment)
chTestResults <- MulEA::calculateChiSquaredTest(model = modeflFromEszterDf,
                                                sampleVector = myDataFromExperiment)

vignette(package = "MulEA")
vignette("MulEA")

cat("\014")
