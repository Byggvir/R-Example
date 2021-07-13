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
LineColors <- rainbow(11,start=0,end=1)

png(paste("png/RKI_CFR_nach_Kw_offset_", week_offset,".png", sep=""), width = 1920, height = 1080)

# Two diagrams side by side

par(   mar = c(10,6,10,6)
     , mfrow = c(1,2)
     )

# Plot a single line for one age group

single_plot <- function(AgeGroup, AGText ) {
  
  # If Agegroup = -1 plot line over all age groups
  # else plot a line for one age group
  
  if (AgeGroup == -1) {
    whereclause1 <- " "
    whereclause2 <- " "
  }  
  else { 
    whereclause1 <- paste( "where AgeGroup div 10 = ", AgeGroup )
    whereclause2 <- paste( "where AgeGroup = ", AgeGroup, " * 10" )
  }
  
  # Get cases from database
  
  SQL <- paste("select (Jahr-2020)*53 + Kw as Kw, sum(CumulatedCount) as Count from RKI_CasesByAge" , whereclause1 , "group by Jahr,Kw;")
  cases <- sqlGetRKI(SQL)
  
  # Get deaths from database
  
  SQL <- paste("select (Jahr-2020)*53 + Kw as Kw, sum(Count) as Count from RKI_DeathsByAgeKw", whereclause2, "group by Jahr,Kw;")
  deaths <- sqlGetRKI(SQL)
  
  l <- nrow(deaths)
  o <- week_offset

  CFR <- deaths$Count[(1+o):l]/cases$Count[1:(l-o)]

  lines(
    cases$Kw[as.numeric(1+o):l]
    , CFR * 100.0
    , type = "l"
    , lwd = 3
    , col = LineColors[AgeGroup+2]
  )

 axis(1,cex.axis=2 )  
 axis(2,cex.axis=2, las = 2 )
 
 CFRMax = max(CFR[15:l])
 
 print(CFRMax*100)
 print(CFR*100)
 
 text( cases$Kw[l] - 2
      , CFR[l]*100 + 2
      , paste(round(CFR[l]*100,1),"%\nmax ",round(CFRMax*100,1),"%",sep='')
      , cex = 1.5
      , adj = 1
      , pos = 2
      , col = LineColors[AgeGroup+2]
      )
}

ylim <-c(0,40)

plot(
  NULL
  , xlab = "Kalenderwoche"
  , ylab = "CFR [%]"
  , xlim = c(15,75)
  , ylim = ylim
  , main = paste("CoViD-19: Rohe CFR nach Kw und Altersgruppe")
  , sub = ""
  , cex.main = 3
  , cex.sub = 1.5
  , cex.lab = 1
  , axes = FALSE 
)

grid()

title(sub = "CFR auf Basis der bis zur Kalenderwoche n kumulierte Fallzahlen", line = 8, cex.sub = 2)
options (digits = 3)

 for (a in 6:9) {
  
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


legend(
  "top"
  , legend = paste("Altersgruppe", AgeGroups[7:10],sep=" ")
  , col = LineColors[7:11]
  , lty = 1
  , lwd = 3
  , cex = 1
)

#--- Alle Altersgruppen ---

ylim <-c(0,10)

plot(
  NULL
  , xlab = "Kalenderwoche"
  , ylab = "CFR [%]"
  , xlim = c(15,75)
  , ylim = ylim
  , main = paste("CoViD-19: Rohe CFR nach Kw alle Altergruppen")
  , sub = ""
  , cex.main = 3
  , cex.sub = 1.5
  , cex.lab = 1
  , axes = FALSE 
)

grid()

title(sub = "CFR auf Basis der bis zur Kalenderwoche n kumulierte Fallzahlen", line = 8, cex.sub = 2)
options (digits = 3)

single_plot(-1,'Alle')

copyright()

dev.off()

