
# .header on
# .mode column
# Link to SQL managera chrome://sqlitemanager/content/sqlitemanager.xul
# PUBLIC API
startLocalDatabase <- function(muleaDBLocalization = ":memory:") {
    driver <- RSQLite::SQLite()
    if (":memory:" == muleaDBLocalization) {
        db <- DBI::dbConnect(drv = driver, dbname = ":memory:")
    } else {
        muleaDBLocalization <- paste(muleaDBLocalization, "MulEA.sqlite", sep = "", collapse = NULL)
        db <- DBI::dbConnect(drv = driver, dbname = muleaDBLocalization)
    }

    assign("databaseConnection", db, envir = .GlobalEnv)
    assign("databaseLocalization", muleaDBLocalization, envir = .GlobalEnv)

    DBI::dbBegin(conn = db)

    DBI::dbSendQuery(conn = db, "CREATE TABLE IF NOT EXISTS organisms_models (
                   taxonomy_id INTERGER NOT NULL,
                   scientific_name TEXT,
                   common_english_name TEXT,
                   model_source TEXT NOT NULL,
                   version INTEGER NOT NULL,
                   description TEXT,
                   PRIMARY KEY (taxonomy_id, model_source, version));")
    DBI::dbCommit(conn = db)
}

stopLocalDatabase <- function() {
    db <- get("databaseConnection", envir = .GlobalEnv);
    DBI::dbDisconnect(conn = db);
    assign("databaseConnection", NULL, envir = .GlobalEnv)
    assign("databaseLocalization", NULL, envir = .GlobalEnv)
}

# PUBLIC API
addModelToLocalDatabase <- function(gmtFilePath, taxonomy_id, model_source, version,
                                    scientific_name = 'NULL', common_english_name = 'NULL',
                                    description = 'NULL') {
    maxColLength <- max(count.fields(gmtFilePath, sep = '\t'))
    model <- read.table(file = gmtFilePath, header = FALSE, fill = TRUE,
                      stringsAsFactors = FALSE, sep = "\t", strip.white = TRUE
                      , col.names = paste0("V",seq_len(maxColLength)))
    db <- get("databaseConnection", envir = .GlobalEnv)
    DBI::dbBegin(conn = db)
    insertEntryToOrganismsModels(taxonomy_id = taxonomy_id, model_source = model_source,
                                 version = version, scientific_name = scientific_name,
                                 common_english_name = common_english_name,
                                 description = description)

    createModelTable(taxonomy_id = taxonomy_id, model_source = model_source, version = version)
    insertEntriesToModelTable(model, taxonomy_id = taxonomy_id, model_source = model_source, version = version)
    DBI::dbCommit(conn = db)
}

insertEntryToOrganismsModels <- function(taxonomy_id, model_source, version
                                         , scientific_name = 'NULL', common_english_name = 'NULL', description = 'NULL') {
    db <- get("databaseConnection", envir = .GlobalEnv)
    scientific_name <- paste("'", scientific_name, "'", sep = "")
    common_english_name <- paste("'", common_english_name, "'", sep = "")
    model_source <- paste("'", model_source, "'", sep = "")
    description <- paste("'", description, "'", sep = "")

    values <- paste(taxonomy_id, scientific_name, common_english_name,
                    model_source, version, description, sep = ", ")

    query <- paste("INSERT INTO organisms_models",
                   "(taxonomy_id, scientific_name, common_english_name, model_source, version, description)",
                   "VALUES (", values, ");", sep = " ")
    print(query)
    DBI::dbSendQuery(conn = db, query)
}

createModelTable <- function(taxonomy_id, model_source, version) {
    db <- get("databaseConnection", envir = .GlobalEnv)

    tableName <- generateModelTableName(taxonomy_id, model_source, version)
    tableDefinition <- "(collection_id TEXT NOT NULL,
                         collection_name TEXT,
                         collection TEXT,
                         PRIMARY KEY (collection_id));"

    query <- paste("CREATE TABLE", tableName, tableDefinition, sep = " ")
    print(query)
    DBI::dbSendQuery(conn = db, query)
}

generateModelTableName <- function(taxonomy_id, model_source, version) {
    tableName <- paste('model', taxonomy_id, model_source, version, sep = "_")
    tableName
}

insertEntriesToModelTable <- function(model, taxonomy_id = taxonomy_id,
                                      model_source = model_source, version = version) {
    db <- get("databaseConnection", envir = .GlobalEnv)

    tableName <- generateModelTableName(taxonomy_id, model_source, version)

    apply(model, 1, FUN = function(dataFrameRow){
        collection_id <- paste("'", dataFrameRow[1], "'", sep = "")
        collection_name <- paste("'", dataFrameRow[2], "'", sep = "")
        maxModelLenght <- length(dataFrameRow)
        collectionValues <- trimws(paste(dataFrameRow[3:maxModelLenght], collapse = "\t"))
        collection <- paste("'", collectionValues, "'", sep = "")
        values <- paste(collection_id, collection_name, collection, sep = ", ")
        query <- paste("INSERT INTO", tableName,
                       "(collection_id, collection_name, collection)",
                       "VALUES (", values, ");", sep = " ")
        print(query)
        DBI::dbSendQuery(conn = db, query)
    })
}

retrieveModelFromLocalDatabase <- function(taxonomy_id, model_source, version) {
    db <- get("databaseConnection", envir = .GlobalEnv)
    DBI::dbBegin(conn = db)

    tableName <- generateModelTableName(taxonomy_id, model_source, version)
    query <- paste("SELECT * FROM", tableName, ";", sep = " ")

    queryResults <- DBI::dbGetQuery(conn = db, query)

    DBI::dbCommit(conn = db)

    queryResults
}

# PUBLIC API
saveModelFromLocalDatabaseToFile <- function(filePath, taxonomy_id, model_source, version) {
    retrievedModel <- retrieveModelFromLocalDatabase(taxonomy_id, model_source, version)

    write.table(file = filePath, x = retrievedModel, sep = "\t", quote = FALSE,
                row.names = FALSE, col.names = FALSE)
}

# PUBLIC API
removeModelFromLocalDatabase <- function(taxonomy_id, model_source, version) {
    db <- get("databaseConnection", envir = .GlobalEnv)
    DBI::dbBegin(conn = db)

    model_source_as_string <- paste("'", model_source, "'", sep = "")
    query1 <- paste("DELETE FROM organisms_models WHERE", "taxonomy_id", "=", taxonomy_id,
                    "AND", "model_source", "=", model_source_as_string,
                    "AND", "version", "=", version, ";", sep = " ")

    DBI::dbSendQuery(conn = db, query1)

    tableName <- paste('model', taxonomy_id, model_source, version, sep = "_")

    query2 <- paste("DROP TABLE", tableName, ";", sep = " ")

    DBI::dbSendQuery(conn = db, query2)

    DBI::dbCommit(conn = db)
}
