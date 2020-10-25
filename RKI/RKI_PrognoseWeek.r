#!/usr/bin/env Rscript
#
#
# Script: RKI_PrognoseWeek.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"RKI_PrognoseWeek"

require(data.table)
library(REST)

setwd("~/git/R-Example")

source("common/rki_download.r")
source("common/rki_sql.r")
source("common/ta_regressionanalysis.r")
source("lib/copyright.r")

# Auswertung insgesamt

daily <- sqlGetRKI()

l <- length(daily[,1])

options ( digits = 4)

WTagAnteile <- WTagAnteil( 
SQL = '
    SELECT dayofweek(date) as WTag, sum(cases)/
              ( select max(cases) - min(cases) from rki where week(date,3) > 40 and week(date,3) <43 ) as Anteil
    FROM (
      SELECT t1.date as date, t1.cases - t2.cases as cases
      FROM rki AS t1
      JOIN rki AS t2
      ON t1.date = adddate(t2.date,1)
    ) AS t3
    where week(date,3) > 40 and week(date,3) < 43
    GROUP BY WTag;
'
)

print(WTagAnteile)
casesperday <- round(daily[l,7]/WTagAnteile[daily[l,4]+1,2]*WTagAnteile[,2],0)
print(c(casesperday,sum(casesperday),mean(casesperday)))

#  Auswertung nach Wochentag und Wochen ab Kw 10

weekdays <- read.csv('data/week.csv', header = TRUE)

png('png/RKI_weekday.png'
    , width = 1920
    , height = 1080
    )
par ( mar = c(10,10,10,10))


boxplot(
     (weekdays[,2]*100 ~ weekdays[,1])
     , ylim = c(0,max(weekdays[,2]))*100
     , main = "Rel. Verteilung der gemeldeten Fälle auf Wochentage ab Kw 10"  
     , sub = ""
     , xlab = ""
     , ylab = "[%]"
     , xaxt = "n"
     , cex.main = 4
     , cex.lab = 3
     , col = "lightgray"
)
title ( sub = "Wochentag"
       , cex.sub = 3
       , line = -2
       )
axis (1
      , at = 1:7      
      , labels = c("Mo","Di","Mi","Do","Fr","Sa","So")
      , cex.axis = 3
      , line = 1
      , lwd = 0
      )
grid()

wtmean <-aggregate(WAnteil~WTag,FUN = mean, data=weekdays)
wtsd <- aggregate(WAnteil~WTag,FUN = sd, data=weekdays)

lines( wtmean[,1]+1
      , wtmean[,2]
      , type = 'l'
      , col = "orange"
      )

lines( wtmean[,1]+1
       , wtmean[,2]-2*wtsd[,2]
       , type = 'l'
       , col = "green"
)

lines( wtmean[,1]+1
       , wtmean[,2]+2*wtsd[,2]
       , type = 'l'
       , col = "red"
)

print(wtmean)
print(round(daily[l,7]/wtmean[daily[l,4]+1,2]*wtmean[,2],0))


dev.off()
