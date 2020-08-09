#!/usr/bin/r

require(data.table)

png("CasesDeathsDelta.png",width=1080,height=1080)
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

updown <- data.frame(
      date=daily$date
    , wday=daily$wday
    , deltaCases=c(0,daily$incCases[2:m]-daily$incCases[1:(m-1)])
    , deltaDeaths=c(0,daily$incDeaths[2:m]-daily$incDeaths[1:(m-1)])
    
)

options(digits=7)
options(scipen=7)
options(Outdec=".")

op = par(mfcol=c(4,2))
colors=c("black","red","green","blue","orange","yellow","cyan")

for ( w in 0:6) {
  plot( updown$date[updown$date%%7==w]
        , updown$deltaCases[updown$date%%7==w]
        , type="b"
        , col=colors[w+1]
        , xlab=paste("Wochentag ", wdays[w+1])
        , ylab="Anzahl"
          )

}

dev.off()

