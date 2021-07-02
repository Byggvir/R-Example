#!/usr/bin/env Rscript
#
#
# Script: RKI_Mortality.r
#
# R script to plot the CoViD-19 mortality in DEU
# Data is retrieved from Robert-Koch-Institut (RKI) and Bundesamt für Statistik
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"RKI_Mortality"

#library(tidyverse)
#library(scales)

setwd("~/git/R-Example")

source('lib/copyright.r')
source('lib/myfunctions.r')
source('common/rki_sql.r')

today <- Sys.Date()
heute <- format(today - 1, "%Y-%m-%d" )

png(filename = paste("png/",MyScriptName, ".png" , sep ="") 
    , width = 1920
    , height = 1080
    )

par(mar=c(10,8,12,8), mfcol = c(3,1) )

options(big.mark = ".", decimal.mark=",")

Cases <- sqlGetRKI('select (AgeGroup div 10)*10 as Agegroup, sum(Count) as Count from RKI_CasesByAge group by AgeGroup div 10;')
Deaths <- sqlGetRKI('select AgeGroup, sum(Count) as Count from(select (AgeGroup div 10) * 10 as Agegroup, Sex, max(Count) as Count from RKI_DeathsByAgeKw group by AgeGroup div 10, Sex) as B group by B.AgeGroup;')
P <- sqlGetRKI('select (Altersgruppe div 10)*10 as AgeGroup, sum(Anzahl) as Count from WPP.WPP20191 where CountryCode=276 and Jahr=2019 group by Altersgruppe div 10;')
P[10,2] <- sum(P[10:11,2])
Population <- P[1:10,]

ylim <- limbounds(Deaths$Count / Population$Count * 100000)*1.2

bp <- barplot( Deaths$Count / Population$Count * 100000
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
               , names.arg = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89", "90+")
               
)

grid()

text( x = bp 
      , y = Deaths$Count / Population$Count * 100000
      , labels = paste(round(Deaths$Count / Population$Count * 100000,1),"(",Deaths$Count,")")
      , pos = 3
      , cex = 2
      , col = "black"

)

text( 0
      , ylim[2]*0.8
      , paste( "Durchschnit: ~", 
          prettyNum(
            round(
              sum(Deaths$Count)/sum(Population$Count)*100000
              ),
            big.mark="." ,decimal.mark = ","
            )
          , sep = "" )
      , cex = 3
      , adj = 0
)

mtext( paste( "Gestorbene gem. RKI Stand", heute, sep=" " )
       , side = 1
       , adj = 1
       , cex = 2
       , line = 5
)

ylim <- limbounds( Cases$Count / Population$Count * 100000)

bp2 <- barplot( Cases$Count / Population$Count * 100000
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
               , names.arg = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89", "90+")
            
)     
text( x = bp2 
      , y = Cases$Count / Population$Count * 100000
      , labels = paste(round(Cases$Count / Population$Count * 100000,1),"\n(",round(Cases$Count,0),")")
      , pos = 1
      , cex = 2
      , col = "black"
      
)
text( 0
      , ylim[2]*0.8
      , paste( "Durchschnit: ~", 
               prettyNum(
                 round(
                   sum(Cases$Count)/sum(Population$Count)*100000,0
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
mtext( paste( "Fälle gem. RKI Stand", heute , sep = " " )
      , side = 1
      , adj = 1
      , cex = 2
      , line = 5
      )

ylim <- limbounds( Deaths$Count / Cases$Count * 100)*1.2

bp2 <- barplot( Deaths$Count / Cases$Count * 100
                , col = "lightblue"
                , xlab = "Altersgruppe"
                , ylab = "%"
                , ylim = ylim
                , main = "CoViD19 DEU: rohe CFR"
                , sub = ""
                , cex.axis = 2
                , cex.names = 2
                , cex.main = 3
                , cex.lab = 3
                , names.arg = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89", "90+")
                
)     
text( x = bp2 
      , y = Deaths$Count / Cases$Count * 100
      , labels = paste(round(Deaths$Count / Cases$Count * 100 ,3),"% \n(",round(Deaths$Count,0),")")
      , pos = 3
      , cex = 2
      , col = "black"
      
)
text( 0
      , ylim[2]*0.8
      , paste( "Durchschnit: ~", 
               prettyNum(
                 round(
                   sum(Deaths$Count)/sum(Cases$Count)*100,2
                 ),
                 big.mark="." ,decimal.mark = ","
               )
               , "%"
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
mtext( paste( "Fälle gem. RKI Stand", heute , sep = " " )
       , side = 1
       , adj = 1
       , cex = 2
       , line = 5
)
copyright()

dev.off()

