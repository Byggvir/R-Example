#!/usr/bin/env Rscript
#
#
# Script: RKI_CasesDeaths.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"RKI_CasesDeaths"


require(data.table)

setwd("~/git/R-Example")
source("common/rki_download.r")
source("lib/copyright.r")

png("png/RKI_Cases.png",width=1920,height=1080)

colors <-c( "red", "yellow", "green", "blue", "black" )

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

daily <- get_rki_tag_csv ()
startdate <- daily$Date[1]

reported <- format(daily$Date[length(daily$Date)], "%d %b %Y")

options(digits=7)
options(scipen=7)
options(Outdec=".")

op = par(mar=c(10,5,10,5))

Tage <- daily$Date

sel <- daily$WTag != 6
Tage[sel] <- NA

barplot( daily$incCases
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

copyright()

grid()

dev.off()
