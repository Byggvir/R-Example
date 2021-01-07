#!/usr/bin/r
#
#
# Script: US_Cases.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"US_Cases"

require(data.table)
setwd("~/git/R-Example")
source("lib/copyright.r")
source("lib/myfunctions.r")

png( "png/US-CasesDeaths.png"
     , width = 1920
     , height = 1080
     )
#pdf("Cases.pdf",paper="a4r",pointsize=0.1)
#svg("CasesDeaths.svg",width=1920,height=1080)

wdays <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
colors <-c( "red", "yellow", "green", "blue", "black" )

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

fconfirmed <- read.csv(file = 'data/US-confirmed.csv', header=TRUE, sep=",")
reported <- format(as.Date("2020-01-21") + max(fconfirmed$day), "%d %b %Y")
cd = data.frame(UID=fconfirmed$UID,date=fconfirmed$day,wday=(fconfirmed$day+2)%%7,confirmed=fconfirmed$count)
rm(fconfirmed)
cases <- aggregate(confirmed~date,FUN=sum,data=cd)
rm(cd)

fdeaths <- read.csv(file = 'data/US-deaths.csv')
dd = data.frame(UID=fdeaths$UID,date=fdeaths$day,wday=(fdeaths$day+2)%%7,deaths=fdeaths$count)
rm(fdeaths)
deaths <- aggregate(deaths~date,FUN=sum,data=dd)
rm(dd)

m <- length(cases$confirmed)
n <- length(deaths$deaths)

daily = data.frame( 
    date=cases$date,
    wday=(cases$date+2)%%7,
    kw=(cases$date+2)%/%7+4,
    incCases=c(0,cases$confirmed[2:m]-cases$confirmed[1:m-1]),
    incDeaths=c(0,deaths$deaths[2:n]-deaths$deaths[1:n-1])
    )

rm(cases)
rm(deaths)

wdayCases   <-  aggregate(incCases~wday,FUN=sum,data=daily)
wdayDeaths  <-  aggregate(incDeaths~wday,FUN=sum,data=daily)

kwCases   <-  aggregate(incCases~kw,FUN=sum,data=daily)
kwDeaths  <-  aggregate(incDeaths~kw,FUN=sum,data=daily)

options(digits=7)
options(scipen=7)
options(Outdec=".")

op = par(mfcol=c(2,1))

Tage <- daily$date
sel <- daily$wday != 6
Tage[sel] <- NA

ylim <- limbounds (daily$incCases)
barplot( daily$incCases
    , ylim=ylim
    #, col=colors[1]
    , main = paste("US daily new cases until ", reported) 
    , sub = paste("Date:", heute )
    , xlab="Day"
    , ylab="Count"
    , names.arg = Tage
    , col=c(rep("lightblue",6),"red")

    )     

grid()

ylim <- limbounds (daily$incDeaths)
barplot( daily$incDeaths
    , ylim=ylim
    #, col=colors[1]
    , main = paste("US daily deaths until ", reported) 
    , sub = paste("Date:", heute )
    , xlab="Day"
    , ylab="Count"
    , names.arg = Tage
    , col=c(rep("lightblue",6),"red")
    )     

grid()

dev.off()

png("png/US-CasesDeaths-kw.png",width=1920,height=1080)

op = par(mfcol=c(2,1))

ylim <- limbounds (kwCases$incCases) * 1.1

kwPlotC <- barplot(  kwCases$incCases
                     , main = paste("US weekly cases until ", reported) 
                     , sub = paste("Date:", heute )
                     , xlab="Week"
                     , ylab="Count"
                     , ylim = ylim
                     , col = "lightblue"
                     , names.arg = kwCases$kw
)
text( kwPlotC
      , kwCases$incCases
      , round(kwCases$incCases)
      , cex = 1
      , pos = 3
      , offset = 2
      , srt = 90
)

ylim <- limbounds (kwDeaths$incDeaths) * 1.1
kwPlotD <- barplot( kwDeaths$incDeaths
                    , main = paste("US weekly deaths until ", reported) 
                    , sub = paste("Date:", heute )
                    , xlab="Week"
                    , ylab="Count"
                    , ylim = ylim
                    , col = "lightblue"
                    , names.arg = kwCases$kw
)

text( kwPlotD
      , kwDeaths$incDeaths
      , round(kwDeaths$incDeaths)
      , cex = 1
      , pos = 3
      , offset = 2
      , srt = 90
)
dev.off()

