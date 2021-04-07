#!/usr/bin/env Rscript
#
#
# Script: RKI.r
#
# last Change: 2021-01-22
#
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"RKI"
PNG <- "png/RKI_CasesDeathsB.png"

setwd("~/git/R-Example")


# Reads the cumulative cases and death from rki.de
# The Excel file is in a very poor format. Therefore we have to adjust the data.
# The daily cases and deaths are in the second worksheet. We need only column 2 and 5.
# The date  in column one is one day ahead in time.

require(data.table)

source("common/rki_sql.r")
source("lib/copyright.r")
source("lib/myfunctions.r")

library(REST)

options( 
    digits=7
  , scipen=7
  , Outdec="."
  , max.print = 3000
  )

daily <- sqlGetRKI()
m <- length(daily[,1])

png(  PNG
    , width=1920
    , height=1080
    )

par(mfcol=c(2,1))

colors <-c( "red", "yellow", "green", "blue", "black" )

today <- Sys.Date()
heute <- format(today, "%d %b %Y")
startdate <- as.Date("2020-02-24")
reported <- daily$Date[m]


barplot( as.numeric(daily$incCases[1:m]) # [fromto]
         , ylim= limbounds(as.numeric(daily$incCases[1:m]))
         , main = paste("Daily cases DE from RKI until ", reported) 
         , sub = ""
         , xlab=""
         , col=c(rep("lightblue",6),"red") 
         , ylab="Anzahl"
         #, names.arg = Tage # [fromto]
         , las = 2
)
title ( sub = paste("Source: rki.de; Created:", heute ), line= 3)

grid()

barplot( as.numeric(daily$incDeaths[1:m]) # [fromto]
         , ylim = limbounds(as.numeric(daily$incDeaths[1:m]))
         , main = paste("Daily deaths DE from RKI until", reported) 
         , sub = ""
         , xlab=""
         , col=c(rep("lightblue",6),"red") 
         , ylab="Anzahl"
         #, names.arg = Tage # [fromto]
         , las = 2
)
title ( sub = paste("Source: rki.de; Created:", heute ), line= 3)

grid()

copyright()

dev.off()
