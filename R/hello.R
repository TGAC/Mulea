.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to Mulea package!")
}

# TODO : Different paths on different OSes. Test it!
runGSEA <- function(databaseFilePath, populationFilePath, sampleFilePath) {
  muleaInstallationDirectory <- find.package("Mulea")
  gseaDirectory <- paste(muleaInstallationDirectory, "/GSEA/", sep = "")
  command <- paste("cd", gseaDirectory, "&&", "./GSEA", databaseFilePath
                   , populationFilePath, sampleFilePath)
  system(command = command)
  resultFilePath <- paste(muleaInstallationDirectory, "/GSEA/Log/result.txt", sep = "")
  conn <- file(resultFilePath, open = "r")
  resultList <- readLines(con = conn)
  resultList
}

# Link to SQL managera chrome://sqlitemanager/content/sqlitemanager.xul
createLocalDatabaseSchema <- function() {
  db <- DBI::dbConnect(RSQLite::SQLite(), dbname = "./Mulea.sqlite")
  DBI::dbSendQuery(conn = db, "CREATE TABLE organisms(taxonomy_id INTERGER, latin_name TEXT);")
  DBI::dbSendQuery(conn = db, "CREATE TABLE ontologies(id INTERGER, type TEXT);")
  DBI::dbSendQuery(conn = db, "CREATE TABLE models(taxonomy_id INTERGER, ontology_id INTERGER,
              set_name TEXT, description TEXT, data TEXT);")
  DBI::dbClearResult(DBI::dbListResults(db)[[1]])
  DBI::dbDisconnect(conn = db)
}

addModelToLocalDatabase <- function(organismTaxonomyId, ontologyType, pathToGMTModelFile) {
  # TODO : Read GMT file to data.frame.
  # TODO : Add file to DB:
  db <- DBI::dbConnect(RSQLite::SQLite(), dbname = "./Mulea.sqlite")
  dbSendQuery(conn = db, "CREATE TABLE organisms(taxonomy_id INTERGER, latin_name Text)")
  DBI::dbDisconnect(conn = db)
}
