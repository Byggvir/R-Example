#!/usr/bin/env Rscript
#
#
# Script: RKI_get_html.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"RKI_get_html"


library(data.table)
library(rvest)
library(stringr)
library(RMariaDB)

insert_new_data <- function (sql="") {
  
  rmariadb.settingsfile <- "/home/thomas/git/R-Example/SQL/COVID19.cnf"
  
  rmariadb.db <- "COVID19"
  
  DB <- dbConnect(RMariaDB::MariaDB(),default.file=rmariadb.settingsfile,group=rmariadb.db)
  
  rsQuery <- dbSendQuery(DB, sql)
  dbClearResult(rsQuery)
  dbDisconnect(DB)
  
}

today <- Sys.Date() - 1

URL <- "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Fallzahlen.html"
all_tables <- read_html(URL) %>%
  html_table(
      fill= TRUE
    , header= TRUE
    )
newdata <- as.numeric(str_replace(all_tables[[1]][18,c(2,6)],'\\.',''))

fallzahlen <- data.table(
    day = today
  , cases = newdata[1]
  , deaths = newdata[2]
)
values <- paste( paste('"',as.character(fallzahlen$day[1]),'"', sep=""), fallzahlen$cases[1], fallzahlen$deaths[1], sep = ',')
print(values)
insert_new_data( paste ("insert into rki values ( ", values ,");"))
                 
