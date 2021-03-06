#!/usr/bin/env Rscript
#
#
# Script: RKI_PredictDeathDE.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"RKI_PredictDeathsDE"


library(tidyverse)
library(scales)

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

setwd("~/git/R-Example")

png(filename = paste("png/", MyScriptName, ".png", sep ="")
    , width = 1920
    , height = 1080
    )
par(mar=c(10,8,12,8))

population <- read.csv("data/DEPopulationAge.csv", sep=";")
CFR <- readODS::read_ods(path = "data/SterbeFälleAlter.ods", sheet = 2)

population$ageband <- population[,"age"] %/% 10

CFRyear <- data.frame(
      Deaths =c( CFR$Male[1:8]+CFR$Female[1:8], sum(CFR$Male[9:11]+CFR$Female[9:11]) )
    , Cases = c( CFR$Cases[1:8], sum(CFR$Cases[9:11]) )
)

options(big.mark = ".", decimal.mark=",")

deaths <- data.frame (
  age = population$age
  , ageband = population$ageband
  , deaths = population$both * CFRyear$Deaths[population$ageband+1]/CFRyear$Cases[population$ageband+1]
)

deathsageband <- aggregate(deaths~ageband, FUN=sum, data=deaths)

bp <- barplot( deathsageband$deaths
               , col = "red"
               , xlab="Altersgruppe"
               , ylab="Anzahl"
               , ylim=c(0,2000000)
               , main = "Prognose Sterbefällen in DEU,\n wenn 83,2 Mio infiziert werden."
               , sub = ""
               , cex.axis = 2
               , cex.names = 2
               , cex.main = 4
               , cex.lab = 3
               , names.arg = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+")
               
)
grid()

text( x = bp 
      , y = deathsageband$deaths
      , labels = prettyNum(round(deathsageband$deaths/1000)*1000,big.mark=".",decimal.mark = ",")
      , pos = 3
      , cex = 2
      , col = "black"

)
text( 3
     , 1500000
     , paste( "Summe: ~", prettyNum(round(sum(deathsageband$deaths)/1000)*1000,big.mark="." ,decimal.mark = ","), sep = "")
     , cex = 5
     , adj = 0
     )
mtext(paste("Stand:",heute),side=3,adj=1, cex = 2, line = 1)

dev.off()
