#!/usr/bin/env Rscript

require(data.table)

setwd("~/git/R-Example")

png("png/CasesDeathsDE.png",width=1920,height=1080)

colors <-c( "red", "yellow", "green", "blue", "black" )

today <- Sys.Date()
heute <- format(today, "%d %b %Y")
fromto <- c(42:102)
startdate <- as.Date("2020-01-20")

daily <- read.csv ( "data/Germany.csv", header = TRUE)
reported <- format(startdate + max(daily$Date), "%d %b %Y")

m <- length(daily$Date)

daily$incCases <- c(0,daily$Cases[2:m]-daily$Cases[1:m-1])
daily$incDeaths <- c(0,daily$Deaths[2:m]-daily$Deaths[1:m-1])

options(digits=7)
options(scipen=7)
options(Outdec=".")

op = par(mar=c(10,5,5,5))

Tage <- daily$Date + startdate

sel <- daily$WTag != 6

Tage[sel] <- NA

    
barplot( daily$incCases # [fromto]
    , ylim=c(0,(max(daily$incCases)%/%1000+1)*1000)
    #, col=colors[1]
    , main = paste("Daily cases DE ", reported) 
    , sub = ""
    , xlab=""
    , col=c(rep("blue",6),"red") 
    , ylab="Anzahl"
    , names.arg = Tage # [fromto]
    , las = 2
    )     
title ( sub = paste("Date:", heute ), line= 3)

grid()

dev.off()