#!/usr/bin/env Rscript
#
#
# Script: RKI_CFR_nach_Kw.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"RKI_CFR_nach_Kw"


# Entwicklung der CFR nach Altersgruppe und Kw

library(data.table)
library(readr)
library(readODS)

setwd("~/git/R-Example")
source("common/rki_download.r")
source("lib/copyright.r")

week_offset <- 2

cumulate <- function(x) {
  
  s <- x
  for (i in 2:length(x)) s[i] <- s[i-1] + x[i]
    
  return (s)
}

deaths <- as.matrix(
    readODS::read_ods(
      path = "data/SterbeFälleAlter.ods" 
      , sheet = 12
  )
)

cases <- as.matrix(readODS::read_ods(path = "data/SterbeFälleAlter.ods", sheet = 3))

AgeGroups <- c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90-99","100+")

png(paste("png/RKI_CFR_nach_Kw_offset_", week_offset,".png", sep=""), width = 3840, height = 2160)

par(   mar = c(10,6,10,6)
     , mfrow = c(3,4)
     )

single_plot <- function(deaths, cases, calweeks, AgeGroup, ylim=c(0,50) ) {
  
  plot(
    calweeks
    , deaths/cases *100
    , type = "l"
    , lwd = 3
    , col = "black"
    , xlab = "Kalenderwoche"
    , ylab = "CFR [%]"
    , ylim = ylim
    , main = paste("Kumulative CoViD-19 CFR nach Kw\nOffset Tote zu Fälle =", week_offset,"Wochen")
    , cex.main = 4
    , cex.lab = 3
    , sub = ""
    , axes = FALSE 
  )
 axis(1,cex.axis=2 )  
 axis(2,cex.axis=2, las = 2 )
 lastweek <- length(calweeks)

 options (digits = 3)
 
 text(
    calweeks[lastweek]
   , ylim[2]/10
   , paste("CFR=", format(deaths[lastweek]/cases[lastweek] * 100, nsmall = 2),"%",
           "\nmax=",format(max(deaths[cases>0]/cases[cases>0]) * 100, nsmall = 2),"%"
           ,sep="")
   , adj = 1
   , cex = 5
   , col= "blue"
 )

 legend(
    "top"
    , legend = paste("Altersgruppe", AgeGroup,sep=" ")
    , col = "black"
    , lty = 1
    , cex = 5
  )
  
 grid()

}

zr1 <- (1 + week_offset):length(cases[,1])
zr2 <- 1:(length(cases[,1]) - week_offset)

for (a in 2:12) {
  
  l <- ((a-2)*10)
  u <- l+9
  
  if (a - 2 == 10) {
    ag <- "100+"
    }
  else {
    ag <- paste(l,"-",u)
  }
  
  single_plot(
      deaths = cumulate(deaths[zr1,a])
    , cases = cumulate(cases[zr1,a])
    , calweeks = cases[zr2,1]
    , AgeGroup = ag

  )
# 
}

AGxPlus <- 2:12

single_plot(
   deaths = cumulate(rowSums(deaths[zr1,AGxPlus]))
  , cases = cumulate(rowSums(cases[zr2,AGxPlus]))
  , calweeks = cases[zr1,1]
  , AgeGroup = "Alle"
  , ylim = c(0,10))

copyright()

dev.off()

options(digits=3)

CFR <- colSums(deaths[,2:12])/colSums(cases[,2:12])

print(colSums(t(cases[,2:12])*CFR))



