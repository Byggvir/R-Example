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

regression_analysis_kw <- function (
  StartKw
  , EndKw
  , StartRegAKw
  , EndRegAKw
  , PrognoseKw
  , data 
  , AG
) {
  
  zr <- data[,1] >= StartRegAKw & data[,1] <= EndRegAKw

  FromTo <- as.numeric(data$Kw[zr] - StartRegAKw)
  
  logdata <- log(data$Count[zr])

  ra1 <- lm(logdata ~ FromTo)
  ci1 <- confint(ra1, level = CI)
  print(ra1)
  
  a <- c(ci1[1, 1], ra1$coefficients[1] , ci1[1, 2])
  b <- c(ci1[2, 1], ra1$coefficients[2] , ci1[2, 2])
  
  xlim <- c(StartRegAKw, PrognoseKw)
  ylim <- c(0
            , ( max(
              c(
                exp(a)
                , exp(a + b * (PrognoseKw - StartRegAKw))
              )
            ) %/% 1000 + 1 ) * 1000 
  )
  
  ylim <- limbounds(data[,2])
  
  png(
    paste(
      "png/RA/AgeGroup-"
      , AG
      , ".png"
      , sep = ""
    )
    , width = 1080
    , height = 1080
  )
  
  par(
    mar = c(10, 5, 10, 5)
  )
  
  plot(
    data[zr,1]
    , data[zr,2]
    , main = ""
    , sub = paste(
      "Regession auf der Basis der Fallzahlen des RKI von der "
      ,
      StartRegAKw
      ,
      ". bis zur "
      ,
      EndRegAKw
      ,
      ". Kw"
      ,
      sep = ""
    )
    , xlab = "Woche"
    , ylab = "Fallzahl pro Woche"
    , xlim = xlim
    , ylim = ylim
    , type = "l"
    , lwd = 3
  )

  zr2 <- data[,1] >= EndRegAKw & data[,1] <= PrognoseKw

  lines ( data[zr2,1]
         , data[zr2,2]
         , col = "gray"
         , lwd = 3
         )
  if (AG <90) {
    Altersband <- paste( AG 
                       , " - "
                       , AG+4)
  }
  else
  {
    Altersband <- '90+'
  }
  
  t <- title (
    main = paste(
      "RA wöchentlichen Fallzahlen der Altersgruppe"
      , Altersband 
    )
    , cex.main = 3
  )
  
  lr <- ifelse (b[2] > 0 ,"left", "right")
  
  legend ( lr
           , legend = c( 
             "Cases inside time window of RA"
             , "Cases outside time window of RA"
             , paste("Upper limit CI ",  CI * 100, "%", sep="")
             , "Mean"
             , paste("Lower limit CI ",  CI * 100, "%", sep="")
           )
           , col = c(
             "black"
             , "gray"
             , "red"
             , "orange"
             , "green"
           )
           , lwd = 2
           , cex = 1
           , inset = 0.05
           , lty = c(1,1,4,3,3,3)
  )
  legend(
    paste("top",lr, sep="")
    , inset = 0.02
    , title = paste( "Wöchentliche Steigerung CI  ",  CI * 100, "%", sep="")
    , legend = c( 
      paste(round((exp(ci1[2,1])-1)*100,1),"%")
      , paste(round((exp(ra1$coefficients[2])-1)*100,1),"%")
      , paste(round((exp(ci1[2,2])-1)*100,1),"%")
    )
    , col = c(
      "green"
      , "orange"
      , "red"
    )
    , lty = 3 
    , lwd = 3
    , cex = 2
  )
  
  grid()
  
  plotregression(a,
                 b,
                 xlim = c(0, PrognoseKw - StartRegAKw),
                 ylim = ylim
                 )

  copyright( c('TAr','RKI'))
  
  dev.off()
}

SQL <- 'call MinMaxCasesPerWeekAgeGroup(20,55);'
AgeGroups <- sqlGetRKI(SQL = SQL)

sw <- 10
ew <- 55
srw <- 30
erw <- 51
pw <- 58


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

  regression_analysis_kw(
    StartKw = sw
    , EndKw = ew
    , StartRegAKw = AgeGroups[i,3]
    , EndRegAKw = AgeGroups[i,5]
    , PrognoseKw = pw
    , data = weekly
    , AG = AgeGroups[i,1]
  )
}

