#
# Download cumulative cases and deaths from rki.de
#


library(xlsx)
library(REST)
library(RCurl)
library(lubridate)

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
                     , 1
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
