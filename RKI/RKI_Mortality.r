#!/usr/bin/env Rscript
#
#
# Script: RKI_Mortality.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"RKI_Mortality"


# R script to plot the CoViD-19 mortality in DEU
# Data is retrieved from Robert-Koch-Institut (RKI) and Bundesamt für Statistik
#
# (c) Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de

#library(tidyverse)
#library(scales)

library(readODS)
source('lib/copyright.r')

MyScriptName <- "RKI_Mortallity"

setwd("~/git/R-Example")

today <- Sys.Date()
heute <- format(today - 1, "%d %b %Y")

png(filename = paste("png/",MyScriptName, ".png" , sep ="") 
    , width = 1920
    , height = 1080
    )

par(mar=c(10,8,12,8), mfcol = c(2,1) )

options(big.mark = ".", decimal.mark=",")

CFR <- readODS::read_ods(path = "data/SterbeFälleAlter.ods", sheet = 2)
Cases <- readODS::read_ods(path = "data/SterbeFälleAlter.ods", sheet = 3)

Kw <- max(Cases[,"Kw"])

Population <- read.csv("data/DEPopulationAge.csv", sep=";")
Population$ageband <- Population[,"age"] %/% 10

PopAgeBand <- aggregate(both~ageband, FUN=sum, data=Population)

CFRyear <- data.frame(
      Deaths = c( CFR$Male[1:8]+CFR$Female[1:8], sum(CFR$Male[9:11]+CFR$Female[9:11]) )
    , Cases = c( CFR$Cases[1:8], sum(CFR$Cases[9:11]) )
    , Population = PopAgeBand [,"both"]
)

deaths <- data.frame (
    age = Population$age
  , ageband = Population$ageband
  , deaths = CFRyear$Deaths[Population$ageband+1]
)

ylim <- c(0,ceiling(max(CFRyear$Deaths / CFRyear$Population)* 1000)*100)

bp <- barplot( CFRyear$Deaths / CFRyear$Population * 100000
               , col = "lightgrey"
               , xlab="Altersgruppe"
               , ylab="Anzahl"
               , ylim = ylim
               , main = "CoViD19 DEU: Gestorbene pro 100.000"
               , sub = ""
               , cex.axis = 2
               , cex.names = 2
               , cex.main = 3
               , cex.lab = 3
               , names.arg = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+")
               
)

grid()

text( x = bp 
      , y = CFRyear$Deaths / CFRyear$Population * 100000
      , labels = paste(round(CFRyear$Deaths / CFRyear$Population * 100000,1),"(",CFRyear$Deaths,")")
      , pos = 3
      , cex = 2
      , col = "black"

)

text( 0
      , 150
      , paste( "Durchschnit: ~", 
          prettyNum(
            round(
              sum(CFRyear$Deaths)/sum(CFRyear$Population)*100000
              ),
            big.mark="." ,decimal.mark = ","
            )
          , sep = "" )
      , cex = 3
      , adj = 0
)

mtext( paste( "Gestorbene gem. RKI vom", heute )
       , side = 1
       , adj = 1
       , cex = 2
       , line = 5
)

ylim <- c(0,ceiling(max(CFRyear$Cases / CFRyear$Population)* 1000)*100)

bp2 <- barplot( CFRyear$Cases / CFRyear$Population * 100000
               , col = "lightblue"
               , xlab = "Altersgruppe"
               , ylab = "Anzahl"
               , ylim = ylim
               , main = "CoViD19 DEU: Fälle pro 100.000"
               , sub = ""
               , cex.axis = 2
               , cex.names = 2
               , cex.main = 3
               , cex.lab = 3
               , names.arg = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+")
            
)     
text( x = bp2 
      , y = CFRyear$Cases / CFRyear$Population * 100000
      , labels = paste(round(CFRyear$Cases / CFRyear$Population * 100000,1),"(",CFRyear$Cases,")")
      , pos = 3
      , cex = 2
      , col = "black"
      
)
text( 0
      , ylim[2]*0.8
      , paste( "Durchschnit: ~", 
               prettyNum(
                 round(
                   sum(CFRyear$Cases)/sum(CFRyear$Population)*100000
                 ),
                 big.mark="." ,decimal.mark = ","
               )
               , sep = "" )
      , cex = 3
      , adj = 0
      )


mtext( paste( "Stand:" , heute )
       , side = 3
       , adj = 1
       , cex = 2
       , line = 1
       )
mtext( paste( "Fälle gem. RKI der ", Kw, ". Kalenderwoche" , sep = "" )
      , side = 1
      , adj = 1
      , cex = 2
      , line = 5
      )

copyright()

dev.off()
