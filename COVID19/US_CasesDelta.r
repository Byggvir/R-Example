#!/usr/bin/env Rscript

require(data.table)

setwd("~/git/R-Example")

wdays <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
colors <-c( "red", "yellow", "green", "blue", "black" )

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

args = commandArgs(trailingOnly=TRUE)

if (length(args)==2) {
  inputfile <- args[1]
  country <- args[2]
} else {
  stop("Two arguments must be supplied (input file, contry).", call.=FALSE)
}

daily <- read.csv(inputfile)

reported <- format(as.Date("2020-01-22") + max(daily$Date), "%d %b %Y")

m <- length(daily$Date)

daily$incCases  <- c(0,daily$Cases[2:m]-daily$Cases[1:(m-1)])
daily$incDeaths <- c(0,daily$deaths[2:m]-daily$deaths[1:(m-1)])

options(digits=7)
options(scipen=7)
options(Outdec=".")

colors=c("black","red","green","blue","orange","darkgreen","cyan")

png( paste("png/CasesDeathsDelta",country,".png",sep="")
  , width = 7000
  , height = 5000
  )

op = par(mfrow=c(5,7),oma=c(8,5,5,5),mar=c(5,4,8,4))

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
          , lwd=3
          , xlab=paste("Wochentag ", wdays[w+1])
          , ylab="Anzahl Fälle"
          , ylim=lim
          , main=paste( "Vergleich mit t-", daysback, " Tage", sep='')
          , cex.main = 3
          , cex.sub = 2
          , cex.lab = 2

          )
    abline(h=0, lty=3)
    
    grid()

  }


}
mtext(paste("Wochentagsvergleich - Wochentag mit Vortag, Tag der Vor- und Vorvorwochen (" ,country, ")" ), outer=TRUE,  cex=5,line=-1)
dev.off()
