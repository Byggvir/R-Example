#!/usr/bin/env Rscript

# Reads the cumulative cases and death from rki.de
# The Excel file is in a very poor format. Therefore we have to adjust teh data.
# The daily cases and deaths are in the second worksheet. We need only column 2 and 5.
# The date  in column one is one day ahead in time.

require(data.table)

library(xlsx)
library(REST)
library(RCurl)

setwd("~/git/R-Example")
source("common/rki_download.r")

options( 
  digits=7
  , scipen=7
  , Outdec="."
  , max.print = 3000
)

png(  "png/CasesDeathsDERKI.png"
      , width=1920
      , height=1080
)

daily <- get_rki_kumtab()
print(daily)
# URL <- 'https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data'

dev.off()