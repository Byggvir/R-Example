library(data.table)

copyrights <- data.table (
  shortname = c("CDC","JHU","RKI","TAr")
  , notice = c(
    " CDC; https://covid.cdc.gov/covid-data-tracker/"
    , "Daten: JHU CSSE COVID-19 Data\nhttps://github.com/CSSEGISandData/COVID-19"
    , "Quelle: Robert Koch-Institut (RKI), dl-de/by-2-0"
    , "(c) 2020 by Thomas Arend, Rheinbach"
  )
)

copyright <- function (holders = c( "RKI", "TAr" )) {
  
  h <- pmatch(holders,copyrights$shortname)
  
  for (i in 1:length(h)) {
    mtext ( text = copyrights$notice[h[i]]
            , side = 1
            , adj = 0
            , line  = 3+i
            , outer = FALSE 
    )
  }
}

copyright_text <- function (holders = c( "RKI", "TAr" )) {
  
  h <- pmatch(holders,copyrights$shortname)
  print(h)
  return(copyrights$notice[h])
}
