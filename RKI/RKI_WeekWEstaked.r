#!/usr/bin/env Rscript
#
#
# Script: RKI.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"RKI_Week_WE"
op <- options(nwarnings = 10000)

# Reads the cumulative cases and death from rki.de
# The Excel file is in a very poor format. Therefore we have to adjust the data.
# The weekly cases and deaths are in the second worksheet. We need only column 2 and 5.
# The date  in column one is one day ahead in time.

require(data.table)

setwd("~/git/R-Example")
source("common/rki_sql.r")
source("lib/copyright.r")
source("lib/myfunctions.r")

library(REST)
library(ggplot2)
library(gridExtra)
library(viridis)
library(hrbrthemes)
library(scales)
library(Cairo)
library(extrafont)
extrafont::loadfonts()

# Stacked


options( 
    digits = 7
  , scipen = 999
  , Outdec = "."
  , max.print = 3000
  )

BL <- c( "West", "Ost")
Bev <- sqlGetRKI(SQL = "select BLWestEast(Id) as Bundesland,sum(Anzahl) as Anzahl from BevBLand group by BLWestEast(Id);")

colors <-c( "red", "yellow", "green", "blue", "black" )

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

SQL = paste ('call CasesPerWeekWE();', sep ="")

weekly <- sqlGetRKI(SQL = SQL)

m <- length(weekly[1,])
reported <- weekly$Kw[m]

weekly$Cases[weekly$Bundesland=='Ost'] <- weekly$Cases[weekly$Bundesland=='Ost'] / Bev[1,2] * 100000
weekly$Cases[weekly$Bundesland=='West'] <- weekly$Cases[weekly$Bundesland=='West'] / Bev[2,2] * 100000

lo <- length(weekly$Cases[weekly$Bundesland=='Ost'])
lw <- length(weekly$Cases[weekly$Bundesland=='West'])

# weekly$Cases[weekly$Bundesland=='Bund'] <- weekly$Cases[weekly$Bundesland=='Bund'] / (Bev[1,2]+Bev[2,2]) * 100000

weekly$Deaths[weekly$Bundesland=='Ost'] <- weekly$Deaths[weekly$Bundesland=='Ost'] / Bev[1,2] * 100000
weekly$Deaths[weekly$Bundesland=='West'] <- weekly$Deaths[weekly$Bundesland=='West'] / Bev[2,2] * 100000
# weekly$Deaths[weekly$Bundesland=='Bund'] <- weekly$Deaths[weekly$Bundesland=='Bund'] / (Bev[1,2]+Bev[2,2]) * 100000

wc <- c(  weekly$Cases[weekly$Bundesland=='Ost'][lo-1]
        , weekly$Cases[weekly$Bundesland=='West'][lw-1] )
p1 <- ggplot(weekly, aes(fill=Bundesland, y=Cases, x=Kw)) +
  geom_bar(position="dodge", stat="identity") +
  geom_hline(  yintercept = wc
             , linetype ="dashed"
             , color = c("black","yellow")
             , size=0.5) +
  # geom_hline(yintercept=weekly$Cases[weekly$Bundesland=='Ost'][lo-1]
  #            , linetype="dashed", 
  #            color = "black", size=0.5) +
  # geom_hline(yintercept=weekly$Cases[weekly$Bundesland=='West'][lw-1]
  #            , linetype="dashed", 
  #            color = "black", size=0.5) +
  scale_fill_viridis(discrete = T) +
  ggtitle("Corona: Inzidenz Bundesländer Ost - West nach Kalenderwoche des Meldedatums") +
  theme_ipsum() +
  xlab("Kalenderwoche") +
  ylab("Gemeldete Fälle pro 100.000 pro Woche")


p2 <- ggplot(weekly, aes(fill=Bundesland, y=Deaths, x=Kw)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Corona: Todesfälle Bundesländer Ost - West nach Kalenderwoche des Meldedatums") +
  theme_ipsum() +
  xlab("Kalenderwoche") +
  ylab("Gemeldete Todesfälle pro 100.000 pro Woche")

gg <- grid.arrange(p1,p2, ncol=1)

plot(gg)

ggsave(plot = gg, file = paste('png/', MyScriptName,".png", sep="")
       , type = "cairo-png",  bg = "white"
       , width = 29.7, height = 21, units = "cm", dpi = 150)

summary(warnings())

