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


creationOfLocalDB <- MulEA::createLocalDatabaseSchema()

addToLocalDB20 <- MulEA::addModelToLocalDatabase("/home/koralgooll/doktorat/Rpackages/mulea/Mulea/example/REACTOME_database_dmel_uniq_PROPER.txt",
                                                taxonomy_id = 9006, model_source = 'File',
                                                version = 0, scientific_name = "real animal",
                                                description = 'Test on real data')

addToLocalDB20 <- MulEA::addModelToLocalDatabase("/home/koralgooll/doktorat/Rpackages/mulea/Mulea/example/REACTOME_database_dmel_uniq_PROPER.txt",
                                                 taxonomy_id = 9006, model_source = 'Reactome',
                                                 version = 0, scientific_name = "some real animal",
                                                 description = 'Just test case')

addToLocalDB20 <- MulEA::addModelToLocalDatabase("/home/koralgooll/doktorat/Rpackages/mulea/Mulea/example/REACTOME_database_dmel_uniq_PROPER.txt",
                                                 taxonomy_id = 9016, model_source = 'File',
                                                 version = 0, scientific_name = "R animal",
                                                 description = 'To REMOVE!')


removeFromLocalDB1 <- MulEA::removeModelFromLocalDatabase(taxonomy_id = 9016, model_source = 'File', version = 0)

getData <- MulEA::saveModelFromLocalDatabaseToFile(filePath = "/home/koralgooll/doktorat/Rpackages/mulea/Mulea/example/savedModel.txt",
                                                   taxonomy_id = 9006, model_source = 'File', version = 0)

vignette(package = "MulEA")
vignette("MulEA")

cat("\014")
