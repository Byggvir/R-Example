#!/usr/bin/r

require(data.table)

png("CasesDeaths.png",width=3840,height=1080)
#pdf("Cases.pdf",paper="a4r",pointsize=0.1)
#svg("CasesDeaths.svg",width=1920,height=1080)

wdays <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
colors <-c( "red", "yellow", "green", "blue", "black" )

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

fconfirmed <- read.csv(file = '../data/US-confirmed.csv', header=TRUE, sep=",")
fdeaths <- read.csv(file = '../data/US-deaths.csv')

reported <- format(as.Date("2020-01-22") + max(fconfirmed$day), "%d %b %Y")


cd = data.frame(UID=fconfirmed$UID,date=fconfirmed$day,wday=(fconfirmed$day+2)%%7,confirmed=fconfirmed$count)
dd = data.frame(UID=fdeaths$UID,date=fdeaths$day,wday=(fdeaths$day+2)%%7,deaths=fdeaths$count)

cases <- aggregate(confirmed~date,FUN=sum,data=cd)
deaths <- aggregate(deaths~date,FUN=sum,data=dd)

m <- length(cases$confirmed)
n <- length(deaths$deaths)

daily = data.frame( 
    date=cases$date,
    wday=(cases$date+2)%%7,
    kw=(cases$date+2)%/%7+4,
    incCases=c(0,cases$confirmed[2:m]-cases$confirmed[1:m-1]),
    incDeaths=c(0,deaths$deaths[2:n]-deaths$deaths[1:n-1])
    )

wdayCases   <-  aggregate(incCases~wday,FUN=sum,data=daily)
wdayDeaths  <-  aggregate(incDeaths~wday,FUN=sum,data=daily)
kwCases   <-  aggregate(incCases~kw,FUN=sum,data=daily)
kwDeaths  <-  aggregate(incDeaths~kw,FUN=sum,data=daily)

options(digits=7)
options(scipen=7)
options(Outdec=".")

op = par(mfcol=c(1,2))

barplot( daily$incCases
    , ylim=c(0,100000)
    #, col=colors[1]
    , main = paste("US daily new cases until ", reported) 
    , sub = paste("Date:", heute )
    , xlab="Day"
    , ylab="Count"
    )     

grid()

barplot( daily$incDeaths
    , ylim=c(0,10000)
    #, col=colors[1]
    , main = paste("US daily deaths until ", reported) 
    , sub = paste("Date:", heute )
    , xlab="Day"
    , ylab="Count"
    )     

grid()

dev.off()

png("CasesDeaths-kw.png",width=3840,height=1080)

op = par(mfcol=c(1,2))

plot(  kwCases
     , type="b"
     , xlab = "Kaldenerwoche"
     , ylab = "Anzahl"
)
plot( kwDeaths
      , type="b"
      , xlab = "Kaldenerwoche"
      , ylab = "Anzahl"
)


dev.off()
