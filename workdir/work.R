res[0]
res[1]
res[2]
res[3]
typeof(res[3])
res[3][2]
X <- read.table(textConnection(res[3]))
Y <- unlist(X)
names(Y) <- NULL
typeof(Y)
X[2]

REACTOME_database_dmel_uniq.txt
rt <- read.table(file = "/home/koralgooll/doktorat/Rpackages/mulea/Mulea/example/REACTOME_database_dmel_uniq.txt", header = FALSE, fill = TRUE)
head(rt)
rt[1,]


db <- DBI::dbConnect(RSQLite::SQLite(), dbname = "./Mulea.sqlite")

DBI::dbSendQuery(conn = db, "INSERT INTO organisms VALUES (9606,'homo sapiens');")
DBI::dbSendQuery(conn = db, "INSERT INTO organisms VALUES (3006,'mouse');")
DBI::dbSendQuery(conn = db, "INSERT INTO organisms VALUES (3130,'muszka');")

DBI::dbSendQuery(conn = db, "ANALYZE;")



# Load your R dataframe
load(file = "~/RProject/sqlite/DATA/Class.rda")

l <- list(taxonomy_id = 2132, latin_name="roxi pospolita")
organism <- data.frame(l)
DBI::dbSendQuery(conn = db, paste("INSERT INTO organisms VALUES (", organism$taxonomy_id, ",'", organism$latin_name, "');"))
DBI::dbListFields(db, "organisms")
DBI::dbListTables(db)
DBI::dbWriteTable(conn = db, name = "new_new_organisms", value = organism, row.names = TRUE)
DBI::dbListFields(db, "new_new_organisms")

# Push to SQLite
DBI::dbWriteTable(conn = db, name = "organisms", value = organism, row.names = FALSE, append = TRUE)




DBI::dbExistsTable(conn = db, name = "orgisms")

result <- DBI::dbListResults(db)[[1]]
DBI::fetch(res = result)

results <- DBI::dbListResults(db)
DBI::fetch(res = results)
DBI::dbDisconnect(conn = db)


n = c(2, 3, 5)
s = c("aa", "bb", "cc")
b = c(TRUE, FALSE, TRUE)
df = data.frame(n, s, b)
df
