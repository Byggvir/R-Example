#!/usr/bin/env Rscript
#
#
# Script: RKI_Testungen.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"RKI_Testungen"

# Load statistics from RKI test report
# Source https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Situationsberichte/2020-08-05-de.pdf?__blob = publicationFile
#
#

library(REST)
require(data.table)

setwd("~/git/R-Example")
# source("common/rki_download.r")
source("common/rki_sql.r")
source("lib/copyright.r")
source("lib/myfunctions.r")

setwd("~/git/R-Example")

tests <- sqlGetRKI(SQL = sqlTestungen, prepare = "set @i := 9;" )
cases <- sqlGetRKI()

l <- length(cases$Kw)

tests$proportion <- tests$Positiv / tests$Testungen 

# Assumed test sensitivity and specifity

sens <- 0.9999
spez <- 0.9995

fp <- 1 - spez
fn <- 1 - sens

tests$prevalence <- ( tests$Positiv - tests$Testungen * fp ) / ( tests$Testungen * sens - tests$Testungen * fp )
tests$TP <- tests$Testungen * tests$prevalence * sens
tests$TN <- tests$Testungen * ( 1 - tests$prevalence ) * sens
tests$FP <- tests$Testungen * ( 1 - tests$prevalence ) * fp
tests$FN <- tests$Testungen * tests$prevalence * fn
tests$Infected <- tests$TP + tests$FN
tests$PPP <- tests$TP / tests$Positiv

Nr <- unique(tests$Nr)

lastNr <- max(Nr)

NewInfected <- aggregate(incCases~Kw,FUN = sum, data = cases)

tests$New <- NewInfected$incCases[2:(length(Nr)+1)]

png(  paste( "png/", MyScriptName, ".png", sep ="") 
    , width = 1920
    , height = 1080
    )

par(family = "sans")

options( 
  digits = 7
  , scipen = 10
  , Outdec = "."
  , max.print = 3000
)

par( mar = c(10, 6, 10, 6))

par(mfcol = c(1,2))

plot( tests$Nr
    , tests$prevalence * 100
    , ylim = c(0,10)
    , type = "b"
    , lwd = 5
    , col = "green"
    , xlab = "Calendar weeks"
    , ylab = ""
    , yaxt = "n"
    , main = "Testungen"
    , sub = paste("Sensitivity ", sens*100, "%; Specificity ", spez*100,"%")
    , cex.main = 3
    , cex.sub = 1
    )

lines(tests$Nr
    , tests$proportion * 100
    , type = "b"
    , lwd = 2
    , col = "blue"
    )

legend (
    "topright"
    , legend = c("Prevalence","Proportion of positive", "Positive predictive power")
    , col = c(
          "green"
        , "blue"
        , "orange")
    , lty = 1
    )

axis( side = 2
      , col = "blue"
    , tick = TRUE
    , col.axis = "blue"
)

title(ylab = "Prevalence [%]", col.lab = "green", las = 0)
title(ylab = "Proportion positive tested [%]", line = 1, col.lab = "blue", las = 0)

grid()

par(new = TRUE)


plot( tests$Nr
    , tests$PPP * 100
    , type = "b"
    , lwd = 2
    , col = "orange"
    , ylim = c(0,100)
    , xaxt = "n"
    , yaxt = "n"
    , xlab = ""
    , ylab = ""
    )

axis( side = 4
    , col = "orange"
    , col.axis = "orange"
    , tick = TRUE
)

mtext ( "Positive Predictive Power (PPP) [%]"
    , side = 4
    , col = "orange"
    , line = 3
    , las = 0
    )

ylim <- limbounds(tests$Infected)

plot( tests$Nr
    , tests$Infected
    , ylim = ylim
    , type = "b"
    , col = "green"
    , xlab = "Calendar weeks"
    , ylab = ""
    , lwd = 3
    , main = "Testungen"
    , sub = paste("Sensitivity ", sens*100,"%; Specificity ", spez*100,"%")
    , cex.main = 3
    , cex.sub = 1
)

lines( tests$Nr
     , tests$Positiv
     , type = "b"
     , col = "blue"
     )

# lines(tests$Nr
#     , tests$FP
#     , type = "b"
#     , col = "red"
#     )

lines( tests$Nr
     , tests$New
     , type = "b"
     , col = "black"
     )

legend (
    "topright"
    , legend = c( 
        "Infected"
      , "Positive"
     # , "False positive"
      , "Testes"
      , "New infected"
      )
    , col = c(  "green"
              , "blue"
      #        , "red"
              , "orange"
              , "black"
              )
    , lty = 1
    )

grid()

title(ylab = "Count", line = 5)

par(new = TRUE)

plot( tests$Nr
    , tests$Testungen
    , ylim = c(0,2000000)
    , type = "b"
    , lwd = 3
    , col = "orange"
    , xaxt = "n"
    , yaxt = "n"
    , xlab = ""
    , ylab = ""
    )

axis( side = 4
    , col.axis = "orange"
    , tick = TRUE
    )

mtext ( "Number of tests"
    , col = "orange"
    , side = 4
    , line = 5
    , las = 0
    )

copyright()

dev.off()

# Ende RKI_Testungen.png

# NeuInfizierte - testungen

png ( paste("png/", MyScriptName, "_nit.png", sep = "" ) 
    , width = 1920
    , height = 1080
    )

par( mar = c(5.1, 10, 4.1, 10)
     , las = 1
     , mfcol = c(1,2)
     )

plot( tests$Nr
    , tests$New/tests$Testungen * 100
    , ylim = limbounds(tests$New/tests$Testungen) * 100
    , type = "b"
    , col = "green"
    , xlab = "Calendar weeks"
    , ylab = "[%]"
    , lwd = 3
    , main = "Neu-Infizierte : Testungen"
    , sub = paste("Deutschland 10 bis", lastNr, "Kalenderwoche")
)

lines(tests$Nr
    , tests$proportion * 100
    , type = "b"
    , lwd = 2
    , col = "blue"
    )

legend (
    "topright"
    , legend = c("Neu-Infizierte","Positive Testungen")
    , col = c("green","blue")
    , lty = 1
    )
grid()

plot( tests$Nr[ tests$Nr > 19 ]
    , tests$New[ tests$Nr > 19 ]/tests$Testungen[ tests$Nr > 19 ] * 100
    , ylim = limbounds(tests$New[ tests$Nr > 19 ]/tests$Testungen[ tests$Nr > 19 ]) *100
    , type = "b"
    , col = "green"
    , xlab = "Calendar weeks"
    , ylab = "[%]"
    , lwd = 3
    , main = "Neu-Infizierte : Testungen"
    , sub = paste("Deutschland 20 bis", lastNr, "Kalenderwoche")
    )
    
lines(tests$Nr[ tests$Nr > 19 ]
    , tests$proportion[ tests$Nr > 19 ] * 100
    , type = "b"
    , lwd = 2
    , col = "blue"
    )

legend (
    "topright"
    , legend = c( "Neu-Infizierte", "Positive Testungen" )
    , col = c("green","blue")
    , lty = 1
    )

grid()

copyright()

dev.off()

# Ende Neuinfizierte - Testungen

# Verhältnis der Testungen zu Neuinfizierten und positiven Testungen

AbNr <-27

png(paste("png/", MyScriptName,"_Rel",AbNr,".png",sep = "")
    , width = 1920
    , height = 1080)


par(mar = c(5.1, 10, 4.1, 10),las = 1)

ylim <- limbounds(c(  tests$New[tests$Nr  >= AbNr]/tests$New[tests$Nr == AbNr]
                      , tests$Positiv[tests$Nr >= AbNr]/tests$Positiv[tests$Nr == AbNr]
                      , tests$Testungen[tests$Nr >= AbNr]/tests$Testungen[tests$Nr == AbNr])) * 100
                  
l1 <- plot( tests$Nr[tests$Nr >= AbNr]
      , tests$New[tests$Nr >= AbNr]/tests$New[tests$Nr == AbNr] * 100
      , ylim = ylim
      , type = "b"
      , col = "red"
      , xlab = "Calendar weeks"
      , ylab = "[%]"
      , lwd = 3
      , main = "Neu-Infizierte, pos. und  ges. Testungen"
      , sub = paste("Deutschland", AbNr,"bis", lastNr, "Kalenderwoche; Quelle: RKI")
      , cex.main = 3
)

l2 <- lines( tests$Nr[tests$Nr >= AbNr]
       , tests$Positiv[tests$Nr >= AbNr]/tests$Positiv[tests$Nr == AbNr] * 100
       , type = "b"
       , col = "orange"
)

l3 <- lines( tests$Nr[tests$Nr >= AbNr]
      , tests$Testungen[tests$Nr >= AbNr]/tests$Testungen[tests$Nr == AbNr] * 100
      , type = "b"
      , col = "blue"
)

text( tests$Nr[tests$Nr >= AbNr]
     , ylim[2]
     , labels = round(tests$New[tests$Nr >= AbNr]/tests$New[tests$Nr == AbNr] * 100,1)
     , cex = 1
     , col = "red"
     )

text( tests$Nr[tests$Nr >= AbNr]
     , ylim[2] * 0.95
     , labels = round(tests$Positiv[tests$Nr >= AbNr]/tests$Positiv[tests$Nr == AbNr] * 100,1)
     , cex = 1
     , col = "orange"
)

text( tests$Nr[tests$Nr >= AbNr]
     , ylim[2] * 0.9
     , labels = round(tests$Testungen[tests$Nr >= AbNr]/tests$Testungen[tests$Nr == AbNr] * 100,1)
     , cex = 1
     , col = "blue"
)

legend (
  "left"
  , inset = 0.05
  , title = paste( "Indize ", AbNr, ". Nr = 100%", sep = "" )
  , legend = c( "Neu-Infizierte","positive Testungen", "gesamt Testungen" )
  , col = c( "red", "orange", "blue")
  , lty = 1
  , cex = 3
)
grid()

copyright()

dev.off()

# Ende relative Neuinfizerte, gesamt und positive Testungen 
# Ende relative Neuiunfizierte, Testungen / positive Testungen
