#!/usr/bin/env Rscript

require(data.table)

setwd("~/git/R-Example")
source("common/rki_download.r")

png("png/CasesDeathsDE.png",width=1920,height=1080)

colors <-c( "red", "yellow", "green", "blue", "black" )

today <- Sys.Date()
heute <- format(today, "%d %b %Y")
fromto <- c(42:102)

options(show.error.messages = FALSE)
try (  rm(daily)
     , silent = TRUE
     )
options(show.error.messages = TRUE)

daily <- get_rki_tag_csv ()
startdate <- daily$Date[1]

reported <- format(daily$Date[length(daily$Date)], "%d %b %Y")

options(digits=7)
options(scipen=7)
options(Outdec=".")

op = par(mar=c(10,5,5,5))

Tage <- daily$Date

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

copyright_rki()

grid()

dev.off()
