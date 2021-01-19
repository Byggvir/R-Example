library(RMariaDB)
library(data.table)

sqlTestungen <- 'select (@i:=@i+1) as Nr, Jahr, Kw, Testungen, Positiv from Testungen;'

sqlGetRKI <- function (
  SQL = ' select 
    t1.date as Date
    , (@i:=@i+1) as Day
    , WEEK(t1.date,3) as Kw
    , WEEKDAY(t1.date) as WTag
    , t1.cases as Cases
    , t1.deaths as Deaths
    , t1.cases-t2.cases as incCases
    , t1.deaths-t2.deaths as incDeaths
    from rki as t1 
    inner join rki as t2 
    on t1.date=adddate(t2.date,1)
    where t1.cases > t2.cases;'
  , prepare="set @i := 1;") {
  
  rmariadb.settingsfile <- "/home/thomas/git/R-Example/SQL/COVID19.cnf"
  
  rmariadb.db <- "COVID19"
  
  COVID19DB <- dbConnect(RMariaDB::MariaDB(),default.file=rmariadb.settingsfile,group=rmariadb.db)
  dbExecute(COVID19DB, prepare)
  rsQuery <- dbSendQuery(COVID19DB, SQL)
  dbRows<-dbFetch(rsQuery)
  # Clear the result.
  
  dbClearResult(rsQuery)
  
  dbDisconnect(COVID19DB)
  
  return(dbRows)
}

#--- Special SQL requests

WTagAnteil <- function (
  SQL = '
    SELECT dayofweek(date) as WTag, sum(cases)/(select max(cases) from rki) as Anteil
    FROM (
      SELECT t1.date as date, t1.cases - t2.cases as cases
      FROM rki AS t1 
      JOIN rki AS t2 
      ON t1.date = adddate(t2.date,1)
    ) AS t3
    GROUP BY WTag
    ORDER BY WTag;
'
){

  
  # rmariadb.settingsfile <- "/home/thomas/git/R-Example/SQL/COVID19.cnf"
  # 
  # rmariadb.db <- "COVID19"
  # 
  # COVID19DB <- dbConnect(RMariaDB::MariaDB(),default.file=rmariadb.settingsfile,group=rmariadb.db)
  # rsQuery <- dbSendQuery(COVID19DB, SQL)
  # dbRows<-dbFetch(rsQuery)
  # # Clear the result.
  # 
  # dbClearResult(rsQuery)
  # 
  # dbDisconnect(COVID19DB)
  
  return(sqlGetRKI(SQL))
  
}
