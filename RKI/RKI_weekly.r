#!/usr/bin/env Rscript
#
#
# Script: RKI_weekly.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"RKI_weekly"


# Reads the cumulative cases and death from rki.de
# The Excel file is in a very poor format. Therefore we have to adjust the data.
# The daily cases and deaths are in the second worksheet. We need only column 2 and 5.
# The date  in column one is one day ahead in time.

library(REST)
require(data.table)

setwd("~/git/R-Example")
source("common/rki_sql.r")
source("lib/copyright.r")

options( 
  digits=7
  , scipen=7
  , Outdec="."
  , max.print = 3000
)

png(  paste("png/", MyScriptName, ".png", sep ="")
      , width=1920
      , height=1080
)

reganalysis <- function (data, zr,  main = "Wöchentliche Fälle DE" ) {
  
  z <- length(zr)
  
  pylim <- c(0,max(data$Count[zr]))
  
  ra1 <- lm(data$Count[zr] ~ data$Kw[zr])
  ra2 <- lm(log(data$Count[zr]) ~ data$Kw[zr])
  
  a <- ra2$coefficients[1]
  b <- ra2$coefficients[2]
  
  plot(
      data$Kw[zr] 
    , data$Count[zr]
       , main = ""
       , sub = paste("Vom", data$Kw[zr[1]], "bis", data$Kw[zr[z]], "Kw")
       , xlab = "Datum"
       , ylab = "Wöchentliche Fälle"
       , ylim = pylim
       , type = "b"
       , lwd = 3
  )
  title ( 
    main = main
    , cex.main = 4)
  
  grid()

  abline( ra1
          , col="red"
          , lwd = 3
  )

  par (new = TRUE)

  curve( exp(a+b*x)
         , from = data$Kw[zr[1]]
         , to = data$Kw[zr[z]]
         , col="green"
         , axes = FALSE
         , xlab = ""
         , ylab = ""
         , ylim = pylim
         , lwd = 3
  )

  legend ( "top"
           , inset = 0.02
           , legend = c("Fälle", "Lineare Regression", "Logarithmische Regression")
           , col = c("black", "red", "green")
           , lwd = 2
           , cex = 2
  
  )

  legend ( "bottom"
           , inset = 0.02
           , legend = c(  paste("f(x)=" , round(ra1$coefficients[1],2),"+", round(ra1$coefficients[2],2), "* (x-7)")
                        , paste("f(x)= exp^(" ,round(ra2$coefficients[1],2),"+", round(ra2$coefficients[2],2), "* (x-7) )" )
                        )
           , col = c("red", "green")
           , lwd = 2
           , cex = 1
           
  )
  
}
# ----

daily <- sqlGetRKI()

par (   mar = c(10,10,10,10)
      , mfcol = c(1,2)
)

zeitraum <- 79:82 - 8
weekly <- aggregate(as.numeric(incCases) ~ Kw, FUN = sum, data = daily)
colnames(weekly) <-  c("Kw","Count")

reganalysis(weekly, zeitraum, main = "Wöchentliche Fälle DE" )

rm(weekly)

zeitraum <- 79:82 - 8
weekly <- aggregate(as.numeric(incDeaths) ~ Kw, FUN = sum, data = daily)
colnames(weekly) <- c("Kw","Count")
reganalysis(weekly, zeitraum, main = "Wöchentliche Sterbefälle DE")

copyright()

dev.off()
