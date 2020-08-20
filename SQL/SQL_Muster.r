#!/usr/bin/env Rscript

library(RMariaDB)

rmariadb.settingsfile <- "/home/thomas/git/R-Example/SQL/COVID19.cnf"

rmariadb.db <- "COVID19"

COVID19DB <- dbConnect(RMariaDB::MariaDB(),default.file=rmariadb.settingsfile,group=rmariadb.db)

query <- "SELECT * FROM Testungen;"
# List tables in the database

dbListTables(COVID19DB)

# Execute the query on the database that we connected to above.

rsQuery <- dbSendQuery(COVID19DB, query)

dbRows<-dbFetch(rsQuery)

print(rsQuery)
print(dbRows)

# Clear the result.

dbClearResult(rsQuery)

dbDisconnect(COVID19DB)

