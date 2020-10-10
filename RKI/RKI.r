#!/usr/bin/env Rscript

# Reads the cumulative cases and death from rki.de
# The Excel file is in a very poor format. Therefore we have to adjust the data.
# The daily cases and deaths are in the second worksheet. We need only column 2 and 5.
# The date  in column one is one day ahead in time.

require(data.table)

setwd("~/git/R-Example")
source("common/rki_download.r")

library(REST)

options( 
    digits=7
  , scipen=7
  , Outdec="."
  , max.print = 3000
  )

daily <- get_rki_tag_csv()
m <- length(daily[,1])

# WTag <- aggregate(cbind(incCases,incDeaths) ~ WTag, FUN=sum, data= daily[100:m,] )
# 
# KorrekturFaktor <- data.table (
#   WTag = 0:6
#   , Faktor = sum(WTag$incCases)/WTag$incCases/7
# )
# 
# print(KorrekturFaktor)
# 
# for (i in 1:m) {
#   daily$incCases[i] <- daily$incCases[i] * KorrekturFaktor$Faktor[daily$WTag[i]+1]
# }
# 
# daily$gCases[1] <- daily$incCases[1]
# 
# for (i in 2:m) {
#   daily$gCases[i] <- daily$incCases[i] * 0.2 + daily$gCases[i-1]*0.8
# }

png(  "png/CasesDeathsDERKI.png"
    , width=1920
    , height=1080
    )

par(mfcol=c(2,1))

colors <-c( "red", "yellow", "green", "blue", "black" )

today <- Sys.Date()
heute <- format(today, "%d %b %Y")
startdate <- as.Date("2020-02-24")
reported <- daily$Date[m]

barplot( daily$incCases[1:m] # [fromto]
         , ylim= c(0,(max(daily$incCases[1:m])%/%1000+1)*1000)
         #, col=colors[1]
         , main = paste("Daily cases DE from RKI until ", reported) 
         , sub = ""
         , xlab=""
         , col=c(rep("blue",6),"red") 
         , ylab="Anzahl"
         #, names.arg = Tage # [fromto]
         , las = 2
)
title ( sub = paste("Source: rki.de; Created:", heute ), line= 3)

grid()

barplot( daily$incDeaths[1:m] # [fromto]
         , ylim= c(0,(max(daily$incDeaths[1:m])%/%100+1)*100)
         #, col=colors[1]
         , main = paste("Daily deaths DE from RKI until", reported) 
         , sub = ""
         , xlab=""
         , col=c(rep("blue",6),"red") 
         , ylab="Anzahl"
         #, names.arg = Tage # [fromto]
         , las = 2
)
title ( sub = paste("Source: rki.de; Created:", heute ), line= 3)

dev.off()
