
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

    DbOperationResult <- tryCatch(
        {
            DBI::dbBegin(conn = db)
            DBI::dbGetQuery(conn = db,
                "CREATE TABLE IF NOT EXISTS organisms_models (
                    taxonomy_id INTERGER NOT NULL,
                    scientific_name TEXT,
                    common_english_name TEXT,
                    model_source TEXT NOT NULL,
                    version INTEGER NOT NULL,
                    description TEXT,
                    PRIMARY KEY (taxonomy_id, model_source, version));")
            DBI::dbCommit(conn = db)
        },
        error = function(c) {
            DBI::dbRollback(conn = db)
            stop(c)
            "error"
        },
        warning = function(c) {
            warning(c)
            "warning"
        },
        message = function(c) "message",
        interrupt = function(c) "interrupt"
    )
    DbOperationResult
}

stopLocalDatabase <- function() {
    db <- get("databaseConnection", envir = .GlobalEnv);
    DbOperationResult <- tryCatch(
        {
            checkIfDbIsRunning(db)
            DBI::dbDisconnect(conn = db);
            assign("databaseConnection", NULL, envir = .GlobalEnv)
            assign("databaseLocalization", NULL, envir = .GlobalEnv)
        },
        error = function(c) {
            DBI::dbRollback(conn = db)
            stop(c)
            "error"
        },
        warning = function(c) {
            warning(c)
            "warning"
        },
        message = function(c) {
            message(c)
            "message"
        },
        interrupt = function(c) "interrupt"
    )
    DbOperationResult
}

checkIfDbIsRunning <- function(db = NULL) {
    if (is.null(db)) {
        message("DB haven't been started yet. Please use MulEA::startLocalDatabase().")
    }
}


# PUBLIC API
addModelToLocalDatabase <- function(model, taxonomy_id, model_source, version,
                                    scientific_name = 'NULL', common_english_name = 'NULL',
                                    description = 'NULL') {
    db <- get("databaseConnection", envir = .GlobalEnv)

    DbOperationResult <- tryCatch(
        {
            checkIfDbIsRunning(db)
            DBI::dbBegin(conn = db)
            insertEntryToOrganismsModels(taxonomy_id = taxonomy_id,
                                         model_source = model_source,
                                         version = version,
                                         scientific_name = scientific_name,
                                         common_english_name = common_english_name,
                                         description = description)
            createModelTable(taxonomy_id = taxonomy_id,
                             model_source = model_source,
                             version = version)
            insertEntriesToModelTable(model,
                                      taxonomy_id = taxonomy_id,
                                      model_source = model_source,
                                      version = version)
            DBI::dbCommit(conn = db)
        },
        error = function(c) {
            DBI::dbRollback(conn = db)
            stop(c)
            "error"
        },
        warning = function(c) {
            warning(c)
            "warning"
        },
        message = function(c) {
            message(c)
            "message"
        },
        interrupt = function(c) "interrupt"
    )
    DbOperationResult
}

insertEntryToOrganismsModels <- function(taxonomy_id, model_source, version
                                         , scientific_name = 'NULL', common_english_name = 'NULL', description = 'NULL') {
    db <- get("databaseConnection", envir = .GlobalEnv)
    scientific_name <- paste("\"", scientific_name, "\"", sep = "")
    common_english_name <- paste("\"", common_english_name, "\"", sep = "")
    model_source <- paste("\"", model_source, "\"", sep = "")
    description <- paste("\"", description, "\"", sep = "")

    values <- paste(taxonomy_id, scientific_name, common_english_name,
                    model_source, version, description, sep = ", ")

    query <- paste("INSERT INTO organisms_models",
                   "(taxonomy_id, scientific_name, common_english_name, model_source, version, description)",
                   "VALUES (", values, ");", sep = " ")
    print(query)
    DBI::dbGetQuery(conn = db, query)
}

createModelTable <- function(taxonomy_id, model_source, version) {
    db <- get("databaseConnection", envir = .GlobalEnv)

    tableName <- generateModelTableName(taxonomy_id, model_source, version)
    tableDefinition <- "(category TEXT NOT NULL,
                         description  TEXT,
                         listOfValues TEXT,
                         PRIMARY KEY (category));"

    query <- paste("CREATE TABLE", tableName, tableDefinition, sep = " ")
    print(query)
    DBI::dbGetQuery(conn = db, query)
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
    category <- paste("\"", dataFrameRow$category, "\"", sep = "")
    description <- dataFrameRow$description
    listOfValuesCollapsed <- trimws(paste(dataFrameRow$listOfValues, collapse = "\t"))
    listOfValuesCollapsedEscaped <- paste("\"", listOfValuesCollapsed, "\"", sep = "")
    listOfValues <- paste(category, description, listOfValuesCollapsedEscaped, sep = ", ")
    query <- paste("INSERT INTO", tableName,
                   "(category, description, listOfValues)",
                   "VALUES (", listOfValues, ");", sep = " ")
    print(query)
    DBI::dbGetQuery(conn = db, query)
  })
}

getModelFromLocalDatabase <- function(taxonomy_id, model_source, version) {
    db <- get("databaseConnection", envir = .GlobalEnv)

    DbOperationResult <- tryCatch(
        {
            checkIfDbIsRunning(db)
            DBI::dbBegin(conn = db)
            tableName <- generateModelTableName(taxonomy_id, model_source, version)
            query <- paste("SELECT * FROM", tableName, ";", sep = " ")
            queryResults <- DBI::dbGetQuery(conn = db, query)
            DBI::dbCommit(conn = db)
            queryResults
        },
        error = function(c) {
            DBI::dbRollback(conn = db)
            stop(c)
            "error"
        },
        warning = function(c) {
            warning(c)
            "warning"
        },
        message = function(c) {
            message(c)
            "message"
        },
        interrupt = function(c) "interrupt"
    )
    DbOperationResult
}

# PUBLIC API
saveModelFromLocalDatabaseToFile <- function(taxonomy_id, model_source, version, gmtFilePath) {
    modelDfFromLocalDB <- getModelFromLocalDatabaseAsDf(taxonomy_id = 9001, model_source = "GO", version = 0)
    saveModelFromDataFrameToGmtFile(modelDF = modelDfFromLocalDB, gmtFilePath = gmtFilePath)
    #retrievedModel <- getModelFromLocalDatabase(taxonomy_id, model_source, version)
    #write.table(file = gmtFilePath, x = retrievedModel, sep = "\t", quote = FALSE, row.names = FALSE, col.names = FALSE)
}

# PUBLIC API
getModelFromLocalDatabaseAsList <- function(taxonomy_id, model_source, version) {
    retrievedModel <- getModelFromLocalDatabase(taxonomy_id, model_source, version)
    retrievedModelList <- as.list(strsplit(retrievedModel$listOfValues, "\t"))
    names(retrievedModelList) <- as.vector(retrievedModel$category)
    retrievedModelList
}

#PUBLIC API
getModelFromLocalDatabaseAsDf <- function(taxonomy_id, model_source, version) {
    retrievedModel <- getModelFromLocalDatabase(taxonomy_id, model_source, version)
    ddply(.data = retrievedModel, .variables = c("category"),
          .fun = function(dfRow) {
              description <- paste("\"", dfRow[,"description"], "\"", sep = "")
              listOfValues <- as.character(unlist(strsplit(dfRow[,"listOfValues"], "\t")))
              modelDf <- data.frame('description' = description,
                                    'listOfValues' = I(list(listOfValues)), stringsAsFactors = FALSE)
              modelDf
    })
}

# PUBLIC API
removeModelFromLocalDatabase <- function(taxonomy_id, model_source, version) {
    db <- get("databaseConnection", envir = .GlobalEnv)

    DbOperationResult <- tryCatch(
        {
            checkIfDbIsRunning(db)
            DBI::dbBegin(conn = db)
            model_source_as_string <- paste("\"", model_source, "\"", sep = "")
            query1 <- paste("DELETE FROM organisms_models WHERE", "taxonomy_id", "=", taxonomy_id,
                            "AND", "model_source", "=", model_source_as_string,
                            "AND", "version", "=", version, ";", sep = " ")
            DBI::dbGetQuery(conn = db, query1)
            tableName <- paste('model', taxonomy_id, model_source, version, sep = "_")
            query2 <- paste("DROP TABLE", tableName, ";", sep = " ")
            DBI::dbGetQuery(conn = db, query2)
            DBI::dbCommit(conn = db)
        },
        error = function(c) {
            DBI::dbRollback(conn = db)
            stop(c)
            "error"
        },
        warning = function(c) {
            warning(c)
            "warning"
        },
        message = function(c) {
            message(c)
            "message"
        },
        interrupt = function(c) "interrupt"
    )
    DbOperationResult
}
