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
library(REST)
library(tidyverse)
library(lubridate)
library(date)

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

regression_analysis <- function (
  DayStart
  , DayEnd
  , DayStartRA
  , DayEndRA
  , DayPrognose
  , data
  , nr
) {
  
  zr <- data[,1] >= DayStartRA & data[,1] <= DayEndRA
  
  FromTo <- as.numeric(data$N[zr])

  logdata <- log(data$Anzahl[zr])
  
  ra1 <- lm(logdata ~ FromTo)
  ci1 <- confint(ra1, level = CI)
  
  a <- c(ci1[1, 1], ra1$coefficients[1] , ci1[1, 2])
  b <- c(ci1[2, 1], ra1$coefficients[2] , ci1[2, 2])
  
  xlim <- c(DayStartRA, DayPrognose)
  ylim <- c(0
            , ( max(
              c(
                exp(a)
                , exp(a + b * (DayPrognose - DayStartRA))
              )
            ) %/% 1000 + 1 ) * 1000 
  )
  ylim <- limbounds(as.numeric(data$Anzahl))
  
  png(
    paste(
      "png/RA/DIVI-"
      , nr
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
    data$Tag
    , data$Anzahl
    , main = ""
    , sub = paste(
      "Regression auf der Basis der Fallzahlen des DIVI von der "
      ,
      DayStartRA
      ,
      ". bis zur "
      ,
      DayEndRA
      ,
      sep = ""
    )
    , xlab = "Tag"
    , ylab = "Fallzahl am Tag"
#    , xlim = xlim
    , ylim = ylim
    , type = "l"
    , lwd = 2
    , col = "gray"
  )
  par (new=TRUE)
  
  plot ( data$N[zr]
          , data$Anzahl[zr]
          , col = "red"
          , lwd = 3
          , xlim=c(1,length(data[,1]))
          , ylim = ylim
          , type = "l"
         , xaxt = "n"
         , yaxt = "n"
         , xlab = ""
         , ylab = ""
         
  )
  
  t <- title (
    main = paste(
      "Regressionsanalyse"
      , 'DIVI'
    )
    , cex.main = 3
  )
  
  lr <- ifelse (b[2] > 0 ,"left", "right")
  
  legend ( 'bottomright'
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
    , title = paste( "Daily increase CI  ",  CI * 100, "%", sep="")
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
  
  plotExpRegression(
        a
      , b
      , From = DayStartRA
      , To = DayEndRA
      , xlim = c(DayStart, DayEnd)
      , ylim = ylim
      )

  copyright( c('TAr'))
  
  dev.off()
}
daily <- sqlGetRKI(SQL = 'select @i:=@i+1 as N,Tag, Anzahl from DIVI;', prepare = "set @i:=0;")

sw <- 1
ew <- length(daily[,1])
srw <- 190
erw <- 250
pw <- 260


  regression_analysis(
    DayStart = sw
    , DayEnd = ew
    , DayStartRA = srw
    , DayEndRA = erw
    , DayPrognose <- pw
    , data = daily
    , nr = 1
)

  sw <- 1
  ew <- length(daily[,1])
  srw <- 240
  erw <- 280
  pw <- length(daily[,1])
  
  
  regression_analysis(
    DayStart = sw
    , DayEnd = ew
    , DayStartRA = srw
    , DayEndRA = erw
    , DayPrognose <- pw
    , data = daily
    , nr = 2
  )

  sw <- 1
  ew <- length(daily[,1])
  srw <- 30
  erw <- 120
  pw <- 160
  
  
  regression_analysis(
    DayStart = sw
    , DayEnd = ew
    , DayStartRA = srw
    , DayEndRA = erw
    , DayPrognose <- pw
    , data = daily
    , nr = 3
  )
