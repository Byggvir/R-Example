#!/usr/bin/env Rscript

require(data.table)

#pdf("Cases.pdf",paper="a4r",pointsize=0.1)
#svg("CasesDeaths.svg",width=1920,height=1080)

wdays <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
colors <-c( "red", "yellow", "green", "blue", "black" )

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

daily <- read.csv(file = '../data/us.csv', header=TRUE, sep=",")

reported <- format(as.Date("2020-01-22") + max(daily$Date), "%d %b %Y")

m <- length(daily$Date)

daily$incCases  <- c(0,daily$Cases[2:m]-daily$Cases[1:m-1])
daily$incDeaths <- c(0,daily$deaths[2:m]-daily$deaths[1:m-1])

options(digits=7)
options(scipen=7)
options(Outdec=".")

colors=c("black","red","green","blue","orange","darkgreen","cyan")

png("CasesDeathsDeltaUS.png" ,width=7000,height=5000)
op = par(mfrow=c(5,7),oma=c(2,1,3,1))

for ( daysback in c(1,7,14,21,28)) {
  
  updown <- data.frame(
    date=daily$Date
    , wday=daily$WTag
    , deltaCases=c(rep(0,daysback),daily$incCases[(daysback+1):m]-daily$incCases[1:(m-daysback)])
    , deltaDeaths=c(rep(0,daysback),daily$incDeaths[(daysback+1):m]-daily$incDeaths[1:(m-daysback)])
  )
  lim=c(min(updown$deltaCases),max(updown$deltaCases))

  for ( w in 0:6) {
    plot( updown$date[updown$date%%7==w]
          , updown$deltaCases[updown$date%%7==w]
          , type="b"
          , col=colors[w+1]
          , xlab=paste("Wochentag ", wdays[w+1])
          , ylab="Anzahl Fälle"
          , ylim=lim
          , main=paste( "Vergleich mit t-", daysback, " Tage", sep='')
          ,lwd=3
          )
    abline(h=0, lty=3)
    
    grid()

  }


}
mtext("Wochentagsvergleich - Wochnetag mit Vortag, Tag der Vor- und Vorvorwochen", outer=TRUE,  cex=1,line=-1)
dev.off()

