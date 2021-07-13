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
  
  SQL = paste ('call CasesPerWeekBL(',i,' );', sep ="")

  weekly <- sqlGetRKI(SQL = SQL)
  m <- length(weekly[,1])
  
  labs <- weekly$Kw
  j20 <- weekly$Kw < 54
  j21 <- weekly$Kw > 53
  
  labs[labs>53] <- labs[j21] - 53
  labs[j20] <- paste(labs[j20],20,sep='/')
  labs[j21] <- paste(labs[j21],21,sep='/')
  
  reported <- weekly$Kw[m]
  y <- as.numeric(weekly$Cases[1:m]) 
  bp1 <- barplot( y # [fromto]
         , ylim = limbounds(y)*1.2
         , main = paste("Weekly cases from calendarweek", weekly$Kw[1], "until", reported) 
         , sub = ""
         , xlab = ""
         , col = "lightblue"
         , ylab = "Anzahl"
         , names.arg = labs
         , las = 2
  )

  title ( sub = BL[i,2], line = 3, cex.sub = 1.5 )

  text( bp1
      , y
      , round(y)
      , cex = 1.5
      , pos = 3
      , offset = 3
      , srt = 90
  )
  abline( h = y[m-1]
          , col = "red"
          , lty = 3
  )
  
  abline( h = max(y)
          , col = "red"
          , lty = 3
  )
  
  grid()

  y <- as.numeric(weekly$Deaths[1:m])
  bp2 <- barplot( y # [fromto]
         , ylim = limbounds(y)*1.2
         , main = paste("Weekly deaths from calendarweek", weekly$Kw[1], "until", reported) 
         , sub = ""
         , xlab = ""
         , col = "lightblue" 
         , ylab = "Anzahl"
         , names.arg = labs
         , las = 2
  )

  title ( sub = BL[i,2], line = 3, cex.sub=1.5)

  text( bp2
      , y 
      , round(y)
      , cex = 1.5
      , pos = 3
      , offset = 2
      , srt = 90
)

abline( h=y[m-1]
        , col = "red"
        , lty = 3
        , lwd = 0.2
)

copyright()

dev.off()
}
