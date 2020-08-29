#!/usr/bin/env Rscript

library(tidyverse)
library(scales)

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

setwd("~/git/R-Example")

png(filename = "png/mortalityDE.png"
    , width = 1920
    , height = 1080
    )

par(mar=c(10,8,12,8), mfcol = c(2,1) )

options(big.mark = ".", decimal.mark=",")

CFR <- readODS::read_ods(path = "data/SterbeFälleAlter.ods", sheet = 2)

Population <- read.csv("data/DEPopulationAge.csv", sep=";")
Population$ageband <- Population[,"age"] %/% 10

PopAgeBand <- aggregate(both~ageband, FUN=sum, data=Population)

CFRyear <- data.frame(
      Deaths = c( CFR$Male[1:8]+CFR$Female[1:8], sum(CFR$Male[9:11]+CFR$Female[9:11]) )
    , Cases = c( CFR$Cases[1:8], sum(CFR$Cases[9:11]) )
    , Population = PopAgeBand [,"both"]
)

deaths <- data.frame (
    age = population$age
  , ageband = population$ageband
  , deaths = CFRyear$Deaths[Population$ageband+1]
)

bp <- barplot( CFRyear$Deaths / CFRyear$Population * 100000
               , col = "blue"
               , xlab="Altersgruppe"
               , ylab="Anzahl"
               , ylim=c(0,200)
               , main = "COVID19: Gestorbene pro 100.000 (Gesamt) in DEU"
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

bp2 <- barplot( CFRyear$Cases / CFRyear$Population * 100000
               , col = "blue"
               , xlab="Altersgruppe"
               , ylab="Anzahl"
               , ylim=c(0,500)
               , main = "COVID19: Fälle pro 100.000 (Gesamt) in DEU"
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
      , 450
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


mtext(paste("Stand:",heute),side=3,adj=1, cex = 2, line = 1)

dev.off()
