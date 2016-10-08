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
MulEA::stopLocalDatabase()

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

addToLocalDB20 <- MulEA::addModelToLocalDatabase(model = modelFromEszter,
                                                 taxonomy_id = 9006, model_source = 'Reactome',
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

getData1 <- MulEA::saveModelFromLocalDatabaseToFile(filePath = "testSave1.gmt",
                                                   taxonomy_id = 9006, model_source = 'Reactome', version = 0)
getData2 <- MulEA::saveModelFromLocalDatabaseToFile(filePath = "testSave2.gmt",
                                                   taxonomy_id = 9016, model_source = 'File', version = 0)
getData3 <- MulEA::saveModelFromLocalDatabaseToFile(filePath = "testSave3.gmt",
                                                   taxonomy_id = 9006, model_source = 'GO', version = 0)


modelFromSave1 <- MulEA::readGmtFileAsDF(gmtFilePath = "testSave1.gmt")
modelFromSave2 <- MulEA::readGmtFileAsDF(gmtFilePath = "testSave2.gmt")
modelFromSave3 <- MulEA::readGmtFileAsDF(gmtFilePath = "testSave3.gmt")

vignette(package = "MulEA")
vignette("MulEA")

cat("\014")
