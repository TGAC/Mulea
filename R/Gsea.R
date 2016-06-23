
# TODO : Test it on user with no privilages.
# TODO : Different paths on different OSes. Test it!
# PUBLIC API
runGSEA <- function(databaseFilePath, populationFilePath, sampleFilePath) {
    muleaInstallationDirectory <- find.package("MulEA")
    gseaDirectory <- paste(muleaInstallationDirectory, "/GSEA/", sep = "")
    command <- paste("cd", gseaDirectory, "&&", "./GSEA", databaseFilePath
                     , populationFilePath, sampleFilePath)
    system(command = command)
    resultFilePath <- paste(muleaInstallationDirectory, "/GSEA/Log/result.txt", sep = "")
    conn <- file(resultFilePath, open = "r")
    resultList <- readLines(con = conn)
    resultList
}
