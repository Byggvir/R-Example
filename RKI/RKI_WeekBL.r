#!/usr/bin/env Rscript
#
#
# Script: RKI.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"RKI"
PNG <- "png/RKI_Week_"

# Reads the cumulative cases and death from rki.de
# The Excel file is in a very poor format. Therefore we have to adjust the data.
# The weekly cases and deaths are in the second worksheet. We need only column 2 and 5.
# The date  in column one is one day ahead in time.

require(data.table)

setwd("~/git/R-Example")
source("common/rki_sql.r")
source("lib/copyright.r")
source("lib/myfunctions.r")

library(REST)

options( 
    digits = 7
  , scipen = 7
  , Outdec = "."
  , max.print = 3000
  )

SQL <- 'select * from Bundesland order by IdBundesland;'
BL <- sqlGetRKI(SQL = SQL)

colors <-c( "red", "yellow", "green", "blue", "black" )

today <- Sys.Date()
heute <- format(today, "%d %b %Y")


for (i in BL[,1]) {

  png(  paste( PNG, BL[i,2],".png", sep="")
        , width = 1920
        , height = 1080
  )
  par(mfcol = c(2,1))
  
  SQL = paste ('call CasesPerWeekBL21(',i,' );', sep ="")

  weekly <- sqlGetRKI(SQL = SQL)
  m <- length(weekly[,1])

  reported <- weekly$Kw[m]
  y <- as.numeric(weekly$Cases[1:m]) 
  bp1 <- barplot( y # [fromto]
         , ylim = limbounds(y)*1.1
         , main = paste("Weekly cases from calendarweek", weekly$Kw[1], "until", reported) 
         , sub = ""
         , xlab = ""
         , col = "lightblue"
         , ylab = "Anzahl"
         , names.arg = weekly$Kw
         , las = 1
  )

  title ( sub = BL[i,2], line = 3, cex.sub = 1.5 )

  text( bp1
      , y
      , round(y)
      , cex = 3
      , pos = 3
      , offset = 1
      , srt = 0
  )
      
  grid()

  y <- as.numeric(weekly$Deaths[1:m])
  bp2 <- barplot( y # [fromto]
         , ylim = limbounds(y)*1.1
         , main = paste("Weekly deaths from calendarweek", weekly$Kw[1], "until", reported) 
         , sub = ""
         , xlab = ""
         , col = "lightblue" 
         , ylab = "Anzahl"
         , names.arg = weekly$Kw
         , las = 1
  )

  title ( sub = BL[i,2], line = 3, cex.sub=1.5)

  text( bp2
      , y 
      , round(y)
      , cex = 3
      , pos = 3
      , offset = 1
      , srt = 0
)

copyright()

dev.off()
}
