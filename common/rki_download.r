#
# Download cumulative cases and deaths from rki.de
#

library(xlsx)
library(REST)
library(RCurl)
library(lubridate)
library(RMariaDB)

# Einlesen der Daten aus den aufbereiteten kummulierten Fällen des RKI,
# die mittels Btach Job heruntergelden und aufbereitet wurden.

# Hier: 
# Download der Tablle der kummulativen Fälle vom RKI und 
# aufbereiten der schlecht formatierten Daten

copyright_rki <- function () {

  mtext( text = paste(   "Quelle: Robert Koch-Institut (RKI), dl-de/by-2-0"
                       , "Diagramm: Thomas Arend, Rheinbach"
                       , paste("Stand:", as.character(Sys.Date()))
                       , sep ="\n"
                       )
        , side = 1
        , adj = 0
        , line  = 4
        , outer = FALSE 
        )
}

get_rki_kumtab <- function () {
  
  today <- Sys.Date()
  heute <- format(today, "%d %b %Y")
  startdate <- as.Date("2020-02-24")
  reported <- heute

  d <- as.numeric(today-startdate) - 1

  URL <- "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Fallzahlen_Kum_Tab.xlsx?__blob=publicationFile"
  dfile <- "/tmp/rki.xlsx"

  # Because read.xlsx2 does not read a file from website 

  download.file(URL, destfile = dfile )

  daily <- read.xlsx2( dfile
                     , 2
                     , header=FALSE
                     , startRow = 4
                     , endRow = 4 + d
                     , colIndex = c(1,2,5)
                     , colClasses = c("Date",rep("integer",2))
                     
  )

  colnames(daily) <- c("Date","Cases","Deaths")
  daily$Deaths[is.na(daily$Deaths)] <- 0
  daily$Date <- startdate + c(0:d)
  
  
  m <- length(daily$Cases)
  
  daily$Kw <- 0:(m-1)%/%7+9
  daily$WTag <- 0:(m-1)%%7
  
  daily$incCases <- c(0,daily$Cases[2:m]-daily$Cases[1:(m-1)])
  daily$incDeaths <- c(0,daily$Deaths[2:m]-daily$Deaths[1:(m-1)])

  while( is.na(daily$Cases[m])) { m <- m-1}
  
  return(daily[1:m,])

}

# Einlesen der Daten aus den aufbereiteten kummulierten Fällen des RKI,
# die mittels Btach Job heruntergelden und aufbereitet wurden.

# Hier: Einlesen aus der MariaDB und aufberieten mittels SQL
# Bug: Die Schnittstelle enthält eien Bug. 
# Komplexe Anfagen führen zu korrupten Daten

get_rki_sql <- function (sql="select * from rki;") {
  
  rmariadb.settingsfile <- "/home/thomas/git/R-Example/SQL/COVID19.cnf"
  
  rmariadb.db <- "COVID19"
  
  COVID19DB <- dbConnect(RMariaDB::MariaDB(),default.file=rmariadb.settingsfile,group=rmariadb.db)
  
  rsQuery <- dbSendQuery(COVID19DB, sql)
  
  dbRows<-dbFetch(rsQuery)
  
  # Clear the result.
  
  dbClearResult(rsQuery)
  
  dbDisconnect(COVID19DB)

  return(dbRows)
}

# Einlesen der Daten aus den aufbereiteten kummulierten Fällen des RKI,
# die mittels Btach Job heruntergelden und aufbereitet wurden.

# Hier: Tägliche Fälle.

get_rki_tag_csv <- function () {
  
  t <- read.csv("data/RKI_nach_Tag.csv", header = TRUE, colClasses = c("Date","integer","integer","integer","integer","integer","integer","integer"))
  return(t)
}

# Einlesen der Daten aus den aufbereiteten kummulierten Fällen des RKI,
# die mittels Btach Job heruntergelden und aufbereitet wurden.

# Hier: Wöchentliche Fälle.

get_rki_kw_csv <- function () {
  
  t <- read.csv("data/RKI_nach_Kw.csv", header = TRUE)
  return(t)
  
}

