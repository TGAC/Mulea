smallResults <- Mulea::runGSEA(
    databaseFilePath = "/home/koralgooll/experiments/GSEA/C/GSEA/Database/small_db.txt"
    , populationFilePath = "/home/koralgooll/experiments/GSEA/C/GSEA/Database/small_pool.txt"
    , sampleFilePath = "/home/koralgooll/experiments/GSEA/C/GSEA/Database/small_sample.txt")

mediumResults <- Mulea::runGSEA(
    databaseFilePath = "/home/koralgooll/experiments/GSEA/C/GSEA/Database/medium_db.txt"
    , populationFilePath = "/home/koralgooll/experiments/GSEA/C/GSEA/Database/medium_pool.txt"
    , sampleFilePath = "/home/koralgooll/experiments/GSEA/C/GSEA/Database/medium_sample.txt")

bigResults <- Mulea::runGSEA(
    databaseFilePath = "/home/koralgooll/experiments/GSEA/C/GSEA/Database/big_db.txt"
    , populationFilePath = "/home/koralgooll/experiments/GSEA/C/GSEA/Database/big_pool.txt"
    , sampleFilePath = "/home/koralgooll/experiments/GSEA/C/GSEA/Database/big_sample.txt")


creationOfLocalDB <- Mulea::createLocalDatabaseSchema()

addToLocalDB20 <- Mulea::addModelToLocalDatabase("/home/koralgooll/doktorat/Rpackages/mulea/Mulea/example/REACTOME_database_dmel_uniq_PROPER.txt",
                                                taxonomy_id = 9006, model_source = 'File',
                                                version = 0, scientific_name = "real animal",
                                                description = 'Test on real data')

addToLocalDB20 <- Mulea::addModelToLocalDatabase("/home/koralgooll/doktorat/Rpackages/mulea/Mulea/example/REACTOME_database_dmel_uniq_PROPER.txt",
                                                 taxonomy_id = 9006, model_source = 'Reactome',
                                                 version = 0, scientific_name = "some real animal",
                                                 description = 'Just test case')

addToLocalDB20 <- Mulea::addModelToLocalDatabase("/home/koralgooll/doktorat/Rpackages/mulea/Mulea/example/REACTOME_database_dmel_uniq_PROPER.txt",
                                                 taxonomy_id = 9016, model_source = 'File',
                                                 version = 0, scientific_name = "R animal",
                                                 description = 'To REMOVE!')


removeFromLocalDB1 <- Mulea::removeModelFromLocalDatabase(taxonomy_id = 9016, model_source = 'File', version = 0)

getData <- Mulea::saveModelFromLocalDatabaseToFile(filePath = "/home/koralgooll/doktorat/Rpackages/mulea/Mulea/example/savedModel.txt",
                                                   taxonomy_id = 9006, model_source = 'File', version = 0)

vignette(package = "Mulea")
vignette("Mulea")
