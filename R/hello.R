# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

invokeCppCodeOnCLI <- function() {
  command <- "cd /home/koralgooll/experiments/GSEA/C/GSEA/ && ./GSEA"
  system(command = command)
}

# Lin to SQL managera chrome://sqlitemanager/content/sqlitemanager.xul
createLocalDatabase <- function() {
  db <- dbConnect(SQLite(), dbname = "./Test.sqlite")
  dbSendQuery(conn = db, "CREATE TABLE School(SchID INTERGER, Location Text, Authority TEXT, SchSize TEXT)")
}
