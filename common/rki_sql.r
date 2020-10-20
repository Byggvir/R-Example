library(RMariaDB)
library(data.table)

WTagAnteil <- function (
  SQL = '
    SELECT weekday(date) as WTag, sum(cases)/(select max(cases) from rki) as Anteil
    FROM (
      SELECT t1.date as date, t1.cases - t2.cases as cases
      FROM rki AS t1 
      JOIN rki AS t2 
      ON t1.date = adddate(t2.date,1)
    ) AS t3
    GROUP BY WTag;
'
) {


  
  rmariadb.settingsfile <- "/home/thomas/git/R-Example/SQL/COVID19.cnf"
  
  rmariadb.db <- "COVID19"
  
  COVID19DB <- dbConnect(RMariaDB::MariaDB(),default.file=rmariadb.settingsfile,group=rmariadb.db)
  rsQuery <- dbSendQuery(COVID19DB, SQL)
  dbRows<-dbFetch(rsQuery)
  # Clear the result.
  
  dbClearResult(rsQuery)
  
  dbDisconnect(COVID19DB)
  
  return(dbRows)
}
