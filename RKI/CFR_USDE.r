#!/usr/bin/env Rscript

library(data.table)
library(utils)
require("readODS")

setwd("~/git/R-Example")

today <- Sys.Date()
heute <- format(today, "%d %b %Y")


US <- read_ods("data/CFR.ods",sheet=6)
DE <- read_ods("data/SterbeFälleAlter.ods",sheet=2)
DE$Deaths <- DE$Male + DE$Female

png("png/CFR_USDE.png", width = 1920, height = 1080)

options( digits = 3 , OutDec = ".")

par ( mar=c(10,10,10,10), mfcol=c(3,2))

stats <- data.table(
  AgeGroup = c("0-49","50+")
  , USCases = c(sum(US$Cases[1:5]),sum(US$Cases[6:length(US$AgeGroup)]))
  , USDeaths = c(sum(US$Deaths[1:5]),sum(US$Deaths[6:length(US$AgeGroup)]))
  , DECases = c(sum(DE$Cases[1:5]),sum(DE$Cases[6:length(DE$Age)]))
  , DEDeaths = c(sum(DE$Deaths[1:5]),sum(DE$Deaths[6:length(DE$Age)]))
)

PlotBar <-  function( x, main ="", ylim=c(0,100)) {

  bp <- barplot(
    x * 100
    , xlab = "Age groups"
    , ylab = "[%]"
    , cex.axis = 2
    , ylim = ylim
    , main = ""
    , col=c("green","red")
    , names.arg = c("0-49","50+")
    , cex.names = 2
  )

  title(
    main = main
    , cex.main = 3
    )

  grid()
  
  text( x = bp
        , y = ylim[2]/20
        , labels = paste(round(x * 100,1), "%", sep="")
        , pos = 3
        , cex = 2
        , col = "black"
        
  )
  
}

PlotBar( 
  stats$USCases / sum(stats$USCases)
  , main = "US Cases by age group [%]"
)

PlotBar( 
  stats$USDeaths / sum(stats$USDeaths)
  , main = "US Deaths by age group [%]"
)

PlotBar( 
  stats$USDeaths / stats$USCases
  , main = "US Case Fatality Ratio (CFR) by age group [%]"
  , ylim = c(0,10)
)

text( 0.5
      , 5
      , paste("Average CFR", sprintf("%0.1f",round(sum(stats$USDeaths)/sum(stats$USCases) * 100, digits = 1)),"%")
      , cex = 2)
     
PlotBar(
  stats$DECases / sum(stats$DECases)
  , main = "DE Cases by age group [%]"
)

PlotBar(
  stats$DEDeaths / sum(stats$DEDeaths)
  , main = "DE Deaths by age group [%]"
)

PlotBar( 
  stats$DEDeaths / stats$DECases
  , main = "DE Case Fatality Ratio (CFR) by age group [%]"
  , ylim = c(0,10)
  
)

text( 0.5
      , 5
      , paste("Average CFR", sprintf("%0.1f",round(sum(stats$DEDeaths)/sum(stats$DECases) * 100, digits = 1)),"%")
      , cex = 2 )

dev.off()

print(stats)