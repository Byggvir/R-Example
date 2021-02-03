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

library(data.table)
library(readr)
library(readODS)

setwd("~/git/R-Example")
source("common/rki_download.r")
source("common/rki_sql.r")
source("lib/copyright.r")
source("lib/myfunctions.r")

week_offset <- 2

cumulate <- function(x) {
  
  s <- x
  for (i in 2:length(x)) s[i] <- s[i-1] + x[i]
    
  return (s)
  
}

SQL <- "select (Jahr-2020)*53 + Kw, AgeGroup div 10 as AgeGroup, sum(Count) as Count from RKI_CasesByAge group by Kw,AgeGroup div 10;"
cases <- sqlGetRKI(SQL)

SQL <- "select (Jahr-2020)*53 + Kw, AgeGroup div 10 as AgeGroup, sum(Count) as Count from RKI_DeathsByAgeKw group by Jahr, Kw, AgeGroup div 10 , Sex;"
deaths <- sqlGetRKI(SQL)

# cases <- as.matrix(readODS::read_ods(path = "data/SterbeFälleAlter.ods", sheet = 3))

AgeGroups <- c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90-99","100+")

png(paste("png/RKI_CFR_nach_Kw_offset_", week_offset,".png", sep=""), width = 3840, height = 2160)

par(   mar = c(10,6,10,6)
     , mfrow = c(3,3)
     )

single_plot <- function(AgeGroup, AGText ) {
  
  
  SQL <- paste("select (Jahr-2020)*53 + Kw, AgeGroup div 10 as AgeGroup, sum(Count) as Count from RKI_CasesByAge where AgeGroup div 10 = ", AgeGroup, "group by Jahr, Kw,AgeGroup div 10;")
  cases <- sqlGetRKI(SQL)
  cases$Count <- cumulate( cases$Count)
  
  SQL <- paste("select (Jahr-2020)*53 + Kw, AgeGroup div 10 as AgeGroup, sum(Count) as Count from RKI_DeathsByAgeKw where AgeGroup div 10 = ", AgeGroup, "group by Jahr,Kw,AgeGroup div 10;")
  deaths <- sqlGetRKI(SQL)
  
  l <- nrow(deaths)
  o <- 10
  CFR <- deaths$Count[(1+o):l]/cases$Count[1:(l-l)]
  ylim <-limbounds(CFR)
  
  plot(
    (1+o):l
    , CFR * 100
    , type = "l"
    , lwd = 3
    , col = "black"
    , xlab = "Kalenderwoche"
    , ylab = "CFR [%]"
    , ylim = ylim * 100
    , main = paste("Kumulative CoViD-19 CFR nach Kw\nOffset Tote zu Fälle =", week_offset,"Wochen")
    , cex.main = 4
    , cex.lab = 3
    , sub = ""
    , axes = FALSE 
  )
  
 axis(1,cex.axis=2 )  
 axis(2,cex.axis=2, las = 2 )
 lastweek <- length(cases$Kw)

 options (digits = 3)
 
  legend(
    "top"
    , legend = paste("Altersgruppe", AGText,sep=" ")
    , col = "black"
    , lty = 1
    , cex = 5
  )
  
 grid()

}

for (a in 0:8) {
  
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

copyright()

dev.off()
