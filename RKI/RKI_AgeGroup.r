#!usr/bin/env Rscript
#
#
# Script: RKI_RegressionAnalysisKw.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"RKI_RAAgeGroup"

library(tidyverse)
library(lubridate)
library(REST)

require(data.table)


setwd("~/git/R-Example")
source("common/rki_sql.r")
source("common/ta_regressionanalysis.r")
source("lib/copyright.r")
source("lib/myfunctions.r")

CI <- 0.95

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

options(
  digits = 7
  ,   scipen = 7
  ,   Outdec = "."
  ,   max.print = 3000
)

# Einlesen der Daten aus den aufbereiteten Fällen des RKI

png(
  paste(
    "png/KwAgeGroup"
    , ".png"
    , sep = ""
  )
  , width = 3840
  , height = 2160
)
par(
  mar = c(10, 5, 10, 5)
  , mfrow = c(5,4)
)

plot_kw <- function (
    data 
  , AG
) {
  
  xlim <- limbounds(data[,1])
  ylim <- limbounds(data[,2])
  
  par(
    mar = c(7, 5, 7, 5)
  )
  
  plot(
    data[,1]
    , data[,2]
    , main = ""
    , sub = ""
    , xlab = "Woche"
    , ylab = "Fallzahl pro Woche"
    , xlim = xlim
    , ylim = ylim
    , type = "l"
    , lwd = 3
  )

  
  if (AG == -1) {
    Altersband <- "Alle Altersgruppen"
  }
  else {
    if (AG <90) {
    Altersband <- paste( AG 
                       , " - "
                       , AG+4)
    }
    else {
      Altersband <- '90+'
    }
  }
  
  t <- title (
    main = paste(
      "Wöchentliche Fallzahlen der Altersgruppe"
      , Altersband 
    )
    , cex.main = 3
  )
  
  grid()

}

SQL <- 'call MinMaxCasesPerWeekAgeGroup(10,72);'
AgeGroups <- sqlGetRKI(SQL = SQL)

for (i in 1:length(AgeGroups[,1])) {
  
  SQL <- paste( 'select 
  (A.Jahr-2020)*53+A.Kw as Kw
  , A.Count as Count 
from RKI_CasesByAge as A 
where 
  A.AgeGroup = '
  , AgeGroups[i,1]
  , ';')
  
  weekly <- sqlGetRKI(SQL = SQL, prepare = "set @i:=1")

  plot_kw(
      data = weekly
    , AG = AgeGroups[i,1]
  )
}

SQL <- paste( 'select 
  (A.Jahr-2020)*53+A.Kw as Kw
  , sum(A.Count) as Count 
from RKI_CasesByAge as A
group by A.Jahr, A.Kw
order by A.Jahr, A.Kw;')

weekly <- sqlGetRKI(SQL = SQL, prepare = "set @i:=1")

plot_kw(
  data = weekly
  , AG = -1
)

copyright( c('TAr','RKI'))

dev.off()
