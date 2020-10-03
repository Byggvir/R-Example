#!/usr/bin/env Rscript

library(data.table)
library(readODS)

setwd("~/git/R-Example")

png(filename = "png/sterbezahlenDE.png"
    , width = 1920
    , height = 1080
    )

Sterbezahlen <- read_ods( 
  "data/12613-0006.ods"
  , sheet = 1
  )

plot( Sterbezahlen$Date
  , Sterbezahlen$Female
  , type = "l"
  )

dev.off()