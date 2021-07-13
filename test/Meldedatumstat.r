#!/usr/bin/env Rscript
#
#
# Script: RKI_RegressionAnalysis.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

options(OutDec=',')

MyScriptName <- "RKI_Landkreis-stat.r"

require(data.table)
library(REST)
library(grid)
library(gridExtra)
library(gtable)
library(lubridate)
library(ggplot2)
library(viridis)
library(hrbrthemes)

setwd("~/git/R-Example/")

source("lib/copyright.r")
source("common/rki_sql.r")

sql <- 'select Meldedatum, count(IdLandkreis) as Lk from ( select IdLandkreis, Meldedatum, sum(Anzahlfall) from RKIFaelle group by IdLandkreis, Meldedatum ) as T group by Meldedatum;'

data <- sqlGetRKI(sql)

png(  paste("png/", MyScriptName, ".png",sep="")
      , width = 1920
      , height = 1080
)
plot(hist(data$Lk, breaks = 42))

dev.off()
  
