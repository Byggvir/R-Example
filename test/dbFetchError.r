library(RMariaDB)

options(max.print = 10000000) 

get_sql <- function (sql="select * from test limit 10000;") {
  
  rmariadb.settingsfile <- "/home/thomas/git/R-Example/SQL/COVID19.cnf"
  
  rmariadb.db <- "COVID19"
  
  DB <- dbConnect(RMariaDB::MariaDB(),default.file=rmariadb.settingsfile,group=rmariadb.db)
  
  rsQuery <- dbSendQuery(DB, sql)
  
  dbRows<-dbFetch(rsQuery)
  
  # Clear the result.
  
  dbClearResult(rsQuery)
  
  dbDisconnect(DB)
  
  return(dbRows)
}
print(get_sql())

dput(get_sql(),file = '/tmp/dput.txt')
