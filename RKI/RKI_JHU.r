#!/usr/bin/env Rscript
#
#
# Script: RKI_JHU.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"RKI_JHU"


# Load statistics from RKI test report
# Source https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Situationsberichte/2020-08-05-de.pdf?__blob=publicationFile
#
#

library(REST)
require(data.table)

options( 
  digits=7
  , scipen=7
  , Outdec="."
  , max.print = 3000
)

setwd("~/git/R-Example")
source("common/rki_download.r")
source("lib/copyright.r")

JHU <- read.csv("data/Germany.csv")
J <- length(JHU$Date)

JHU$incCases <- c(0, JHU$Cases[2:J]-JHU$Cases[1:(J-1)])
JHU$incDeaths <- c(0, JHU$Deaths[2:J]-JHU$Deaths[1:(J-1)])

RKI <- get_rki_tag_csv()
R <- length(RKI$Date)

print(c(R,J))

RKI$incCases <- c(0, RKI$Cases[2:R]-RKI$Cases[1:(R-1)])
RKI$incDeaths <- c(0, RKI$Deaths[2:R]-RKI$Deaths[1:(R-1)])

delta <- JHU$incCases[(J-R+1):J] -  RKI$incCases

png("png/RKI_JHU.png"
    , width=1920
    , height=1080)

# par ( mfcol = c(1,2) )

ylim <- c(-2000,8000)

plot(1:R
     , JHU$incCases[(J-R+1):J]
     , type = 'l'
     , col = "blue"
     , xlim = c(1,R)
     , ylim = ylim
     , xlab = "Tage"
     , ylab = "Anzahl"
     , main = "Vergleich CoViD-19 Fallzahlen JHU / RKI"
)
legend("top"
       , legend = c("JHU", "RKI", "JHU-RKI")
       , col = c("blue", "green", "red")
       , cex = 4
       , lwd = 2
)

copyright(c("JHU","RKI","TAr"))

grid()

par ( new = TRUE )

plot( 1:R
     , RKI$incCases
     , type = 'l'
     , col = "green"
     , xlim = c(1,R)
     , ylim = ylim
     , xlab = ""
     , ylab = ""
)

delta <- JHU$incCases[(J-R+1):J] -  RKI$incCases

par ( new = TRUE )

plot( 1:R
      , delta
      , type = 'l'
      , col = "red"
      , xlim = c(1,R)
      , ylim = ylim
      , xlab = ""
      , ylab = ""
)

print(sum(delta))

print((sum(JHU$incCases)-sum(RKI$incCases))/sum(RKI$incCases))
      
dev.off()
