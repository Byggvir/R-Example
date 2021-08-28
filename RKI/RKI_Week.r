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

fPNG <- "png/RKI_Week_Bund.png"

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
SQL = 'call CasesPerWeek()'

weekly <- sqlGetRKI(SQL = SQL)
m <- length(weekly[,1])
reported <- weekly$Kw[m]


png(  fPNG
    , width = 1920
    , height = 1080
    )

par(mfcol = c(2,1))

colors <-c( "red", "yellow", "green", "blue", "black" )

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

y <- as.numeric(weekly$Cases[1:m])

labs <- weekly$Kw
j20 <- weekly$Kw < 54
j21 <- weekly$Kw > 53

labs[labs>53] <- labs[j21] - 53
labs[j20] <- paste(labs[j20],20,sep='/')
labs[j21] <- paste(labs[j21],21,sep='/')


bp1 <- barplot( y # [fromto]
         , ylim = limbounds(y)*1.1
         , main = paste("Wöchentliche Fälle von Kalenderwoche", weekly$Kw[1], "bis", reported) 
         , sub = ""
         , xlab = ""
         , col = "lightblue"
         , ylab = "Anzahl"
         , names.arg = labs
         , las = 2
)

title ( sub = paste("Source: rki.de; Created:", heute ), line = 3)

text( bp1
      , y
      , round(y)
      , cex = 1
      , pos = 3
      , offset = 3
      , srt = 90
)

abline(h=y[m-1] , col = 'red')

abline(h=max(y) , col = 'red')

grid()

y <- as.numeric(weekly$Deaths[1:m])
bp2 <- barplot( y # [fromto]
         , ylim = limbounds(y)*1.1
         , main = paste("Weekly deaths DE from calendarweek", weekly$Kw[1], "until", reported) 
         , sub = ""
         , xlab = ""
         , col = "lightblue" 
         , ylab = "Anzahl"
         , names.arg = labs
         , las = 2
)

abline(h=y[m-1] , col = 'red' , lty = 3)
abline(h=max(y) , col = 'red' , lty = 3)

title ( sub = paste("Source: rki.de; Created:", heute ), line = 3)

text( bp2
      , y 
      , round(y)
      , cex = 1
      , pos = 3
      , offset = 1
      , srt = 90
)

copyright()

dev.off()
