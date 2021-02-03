#!usr/bin/env Rscript
#
#
# Script: RKI_RegressionAnalysisKw.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"RKI_Bundesland"

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

# Einlesen der Daten aus den aufbereiteten kummulierten Fällen des RKI

regression_analysis_kw <- function (
  StartKw
  , EndKw
  , StartRegAKw
  , EndRegAKw
  , PrognoseKw
  , data 
  , Bundesland
) {
  
  zr <- data[,2] >= StartRegAKw & data[,2] <= EndRegAKw
  
  FromTo <- as.numeric(data$Kw[zr] - StartRegAKw) 
  FromTo <- data[zr,2] - StartRegAKw
  
  logdata <- log(data$Cases[zr])
  
  ra1 <- lm(logdata ~ FromTo)
  ci1 <- confint(ra1, level = CI)
  
  print( paste( '---', Bundesland , '---'))
  
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
  ylim <- limbounds(data$Cases)* 2
  
  png(
    paste(
      "png/RA/BL-"
      , Bundesland
      , ".png"
      , sep = ""
    )
    , width = 1920
    , height = 1080
  )
  
  par(
    mar = c(10, 5, 10, 5)
  )
  
  plot(
    data$Kw[zr]
    , data$Cases[zr]
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

  zr2 <- data$Kw >= EndRegAKw & data$Kw <= PrognoseKw

  lines ( data$Kw[zr2]
         , data$Cases[zr2]
         , col = "gray"
         , lwd = 3
         )

  t <- title (
    main = paste(
      "Regressionsanalyse"
      , Bundesland
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


SQL <- 'call MinMaxCasesPerWeek(20,56);'
BL <- sqlGetRKI(SQL = SQL)

sw <- 10
ew <- 58
srw <- 30
erw <- 51
pw <- 56

for (B in BL[,1]) {
  
  SQL <- paste( 'call CasesPerWeekBL('
  , B
  , ');')
  
  weekly <- sqlGetRKI(SQL = SQL, prepare = "set @i:=1")
  
  
regression_analysis_kw(
  StartKw = sw
  , EndKw = ew
  , StartRegAKw = BL[B,4]
  , EndRegAKw = BL[B,6]
  , PrognoseKw <- pw
  , data = weekly
  , Bundesland = BL[B,2]
  )

}


BL <- c( 'Ost', 'West')

for ( B in 0:1 ) {
  SQL <- paste( 'call CasesPerWeekBLWE('
              , B
              , ');')

  weekly <- sqlGetRKI(SQL = SQL, prepare = "set @i:=1")
  regression_analysis_kw(
    StartKw = sw
    , EndKw = ew
    , StartRegAKw = srw
    , EndRegAKw = erw
    , PrognoseKw <- pw
    , data = weekly
    , Bundesland = BL[B+1]
)
}

