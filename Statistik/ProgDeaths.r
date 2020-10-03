#!/usr/bin/r

setwd("~/git/R-Example")

population <- data.readcsv("data/DEPopulationAge.csv")
CFR <- data.readcsv("data/DE_CFR.csv")

aCFR <- rep(0,101)

for ( i in 1:101 ) {
  aCFR[i] <- CFR$Deaths_m[i %/% 10+1]/10
}

print (aCFR)
