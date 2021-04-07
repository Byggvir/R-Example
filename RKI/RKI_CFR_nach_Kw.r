#!/usr/bin/env Rscript
#
#
# Script: RKI_CFR_nach_Kw.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

options(scipen = 999)
MyScriptName <-"RKI_CFR_nach_Kw"


# Entwicklung der CFR nach Altersgruppe und Kw
library(bit64)
library(data.table)
library(readr)
library(readODS)
library(scales)
library(Cairo)
library(extrafont)
extrafont::loadfonts()

setwd("~/git/R-Example")

source("common/rki_sql.r")
source("lib/copyright.r")
source("lib/myfunctions.r")

week_offset <- 0

AgeGroups <- c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90-99","100+")
LineColors <- rainbow(10,start=0,end=1)

png(paste("png/RKI_CFR_nach_Kw_offset_", week_offset,".png", sep=""), width = 1920, height = 1080)

par(   mar = c(10,6,10,6)
     , mfrow = c(1,1)
     )

single_plot <- function(AgeGroup, AGText ) {
  
  SQL <- paste("select (Jahr-2020)*53 + Kw as Kw, sum(CumulatedCount) as Count from RKI_CasesByAge where AgeGroup div 10 = ", AgeGroup, "group by Jahr,Kw;")
  cases <- sqlGetRKI(SQL)

  SQL <- paste("select (Jahr-2020)*53 + Kw as Kw, sum(Count) as Count from RKI_DeathsByAgeKw where AgeGroup = ", AgeGroup, " * 10 group by Jahr,Kw;")
  deaths <- sqlGetRKI(SQL)
  
  l <- nrow(deaths)
  o <- week_offset

  CFR <- deaths$Count[(1+o):l]/cases$Count[1:(l-o)]
  ylim <-c(0,50) #limbounds(CFR)

  lines(
    cases$Kw[as.numeric(1+o):l]
    , CFR * 100.0
    , type = "l"
    , lwd = 3
    , col = LineColors[AgeGroup+1]
  )

 axis(1,cex.axis=2 )  
 axis(2,cex.axis=2, las = 2 )
 
 CFRMax = max(CFR[15:l])
 
 #print(CFRMax*100)
 #print(CFR[l]*100)
 
 text( cases$Kw[l]
      , CFR[l]*100
      , paste(round(CFR[l]*100,1),"% , max ",round(CFRMax*100,1),"%",sep='')
      , cex = 2
      , adj = 1
      , pos = 1
      , col = LineColors[AgeGroup+1]
      )
}

plot(
  NULL
  , xlab = "Kalenderwoche"
  , ylab = "CFR [%]"
  , xlim = c(15,70)
  , ylim = c(0,50)
  , main = paste("CoViD-19: Rohe CFR nach Kw")
  , sub = ""
  , cex.main = 4
  , cex.sub = 3
  , cex.lab = 2
  , axes = FALSE 
)


grid()

title(sub = "Jeweils bis zur Kalenderwoche n kumulierte Fallzahlen", line = 8, cex.sub = 3)
for (a in 5:9) {
  
  l <- a*10
  u <- l+9
  
  if (a == 10) {
    ag <- "100+"
    }
  else {
    ag <- paste(l,"-",u)
  }
  
  single_plot(a,ag)

}
options (digits = 3)

legend(
  "top"
  , legend = paste("Altersgruppe", AgeGroups[6:10],sep=" ")
  , col = LineColors[6:10]
  , lty = 1
  , cex = 2
)


copyright()

dev.off()

