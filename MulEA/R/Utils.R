
# PUBLIC API
readGmtFileAsDF <- function(gmtFilePath) {
    fileConnection <- file(gmtFilePath)
    lines <- readLines(fileConnection)
    close(fileConnection)
    gmtAsDF <- plyr::adply(.data = lines, .margins = 1, .fun = function(line) {
        fields <- strsplit(line, split = "\t")[[1]]
        category <- fields[1]
        if (startsWith(fields[2], "\"") && endsWith(fields[2], "\"")) {
            description <- fields[2]
        } else {
            description <- paste("\"", fields[2], "\"", sep = "")
        }
        listOfValues <- fields[3:length(fields)]
        data.frame('category' = category, 'description' = description, 'listOfValues' = I(list(listOfValues)), stringsAsFactors = FALSE)
    })
    gmtAsDF[c("category", "description", "listOfValues")]
}

#TODO : Is that hepler needed?
readGmtFileAsPlaneDF <- function(gmtFilePath) {
    maxColLength <- max(count.fields(gmtFilePath, sep = '\t', quote = "\""))
    model <- read.table(file = gmtFilePath, header = FALSE, fill = TRUE,
                        stringsAsFactors = FALSE, sep = "\t", strip.white = TRUE,
                        col.names = paste0("V",seq_len(maxColLength)), quote = "\"")
    model
}

saveModelFromDataFrameToGmtFile <- function(modelDF, gmtFilePath) {
    vectorOfModel <- plyr::daply(.data = modelDF, .variables = c("category"), .fun = function(dataFrameRow){
        collapsedListOfValues <- paste(dataFrameRow[,3][[1]], collapse = "\t")
        paste(dataFrameRow[1], dataFrameRow[2], collapsedListOfValues, sep = "\t")
    })
    fileConnection <- file(gmtFilePath)
    writeLines(vectorOfModel, con = fileConnection, sep = "\n", useBytes = FALSE)
    close(fileConnection)
}
