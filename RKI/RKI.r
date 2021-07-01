#!/usr/bin/env Rscript
#
#
# Script: RKI.r
#
# last Change: 2021-06-25
#
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"RKI"
PNG <- "png/RKI_CasesDeaths"

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

diagram <- function (
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
    where t1.cases > t2.cases
    and t1.date >= "2020-02-24";'
  , main = "CoViD-19 DE: Tägliche vom RKI gemeldete Fälle bis"
  , N = 1
) {
  
daily <- sqlGetRKI(SQL = SQL)

m <- length(daily[,1])

png(  paste( PNG, N, '.png', sep="" )
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
         , main = "" 
         , sub = ""
         , xlab=""
         , col=c(rep("lightblue",5),"red","lightblue")
         , ylab="Anzahl"
         #, names.arg = Tage # [fromto]
         , las = 2
)

title ( 
  main = paste( main, "Fälle bis", reported) 
  , line = 3
  , cex  = 5
)

title ( 
  sub = paste("Source: rki.de; Created:", heute )
  , line= 3
  , cex = 4
)

grid()

barplot( as.numeric(daily$incDeaths[1:m]) # [fromto]
         , ylim = limbounds(as.numeric(daily$incDeaths[1:m]))
         , main = ""
         , sub = ""
         , xlab=""
         , col=c(rep("lightblue",5),"red","lightblue") 
         , ylab="Anzahl"
         #, names.arg = Tage # [fromto]
         , las = 2
)
title ( 
  main = paste( main, "Todesfälle bis", reported) 
  , line = 3
  , cex  = 5
)

title ( 
  sub = paste("Source: rki.de; Created:", heute )
  , line= 3
  , cex = 4
  )

grid()

copyright()

dev.off()

}

# daily <- sqlGetRKI('select Meldedatum as Date,count(AnzahlFall) as incCases, count(AnzahlTodesfall) as incDeaths from RKIFaelle where Meldedatum >="2020-02-24" group by Meldedatum;')

diagram( main = "CoViD-19 DE: Tägliche vom RKI gemeldete"
          , N = "A" )

diagram( SQL = 'select Meldedatum as Date,sum(AnzahlFall) as incCases, sum(AnzahlTodesfall) as incDeaths from RKIFaelle where Meldedatum >="2020-02-24" group by Meldedatum;'
          , main = "CoViD-19 DE: Tägliche beim Gesundheitsamt gemeldete"
          , N = "B" )
